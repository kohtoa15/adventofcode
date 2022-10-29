use std::num::ParseIntError;


fn read_hex_data(input: String) -> Result<Vec<u8>, ParseIntError> {
    let mut data = Vec::with_capacity(input.len() / 2);
    let mut pos = 2;
    while pos <= input.len() {
        let start = pos - 2;
        let substr = &input[start..pos];
        let num = u8::from_str_radix(substr, 16)?;
        data.push(num);
        pos += 2;
    }
    // if remainder stays, push it as half filled u8
    if pos == (input.len() + 1) {
        let start = pos - 2;
        let substr = &input[start..input.len()];
        let num = u8::from_str_radix(substr, 16)?;
        // left shift single hex digit value
        data.push(num << 4);
    }
    Ok(data)
}

fn poll_bits(value: &[u8], bits_offset: usize, bits_count: usize) -> u8 {
    assert!(bits_count <= 8);

    let offset_bytes = bits_offset / 8;
    let offset_bits = bits_offset % 8;
    // generate bitmask
    let mut bitmask: u16 = 0;
    for pos in 0..bits_count {
        let bit: u16 = 0x8000 >> pos;
        bitmask += bit;
    }
    bitmask = bitmask >> offset_bits;
    // combine two value bytes to get data from
    let fst_offset_byte = value.get(offset_bytes).map(|b| *b).unwrap_or(0);
    let snd_offset_byte = value.get(offset_bytes + 1).map(|b| *b).unwrap_or(0);
    let offset_value: u16 = ((fst_offset_byte as u16) << 8) + (snd_offset_byte as u16);
    // combine bitmask with value at offset
    let value = bitmask & offset_value;
    // shift resulting value as far right as possible
    return (value >> (16 - bits_count - offset_bits)) as u8;
}

#[test]
fn test_poll_bits() {
    {
        let data: Vec<u8> = vec![0, 0, 0, 0x0f, 0x00];
        assert_eq!(poll_bits(data.as_slice(), 28, 4), 0x0f);
        assert_eq!(poll_bits(data.as_slice(), 28, 8), 0xf0);
        assert_eq!(poll_bits(data.as_slice(), 28, 2), 0b11);
        assert_eq!(poll_bits(data.as_slice(), 27, 2), 0b1);
        assert_eq!(poll_bits(data.as_slice(), 31, 2), 0b10);
    }
}

// automatically increases the supplied offset by the count of bits we requested
fn poll_bits_increase_offset(value: &[u8], bits_offset: &mut usize, bits_count: usize) -> u8 {
    let ret = poll_bits(value, *bits_offset, bits_count);
    *bits_offset += bits_count;
    return ret;
}

#[test]
fn test_poll_bits_increase_offset() {
    let input_str: &str = "11101110000000001101010000001100100000100011000001100000";
    let input_chars = input_str.len();
    println!("{}", input_str);
    let input = u64::from_str_radix(input_str, 2).unwrap();
    println!("{:064b}", input);

    let data = {
        // make u8 vec from input int
        let mut d = input.to_le_bytes().to_vec();
        // ordering is wrong per default, fix with reversing data
        d.reverse();
        // add 2 empty reserve bytes
        d.extend([0, 0]);
        d
    };

    println!("{}", data.as_slice().iter().map(|n| format!("{:08b}", n)).collect::<Vec<String>>().join(", "));
    // modify inital offset due account for reshifting during byte-ordering
    let mut offset: usize = 64 - input_chars;
    println!("initial offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 3), 7);    // 0b111
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 3), 3);    // 0b011
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 1), 1);    // 0b1
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 8), 0);    // 0b00000000
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 3), 3);    // 0b011
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 8), 80);   // 0b01010000
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 3), 1);    // 0b001
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 8), 144);  // 0b10010000
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 3), 2);    // 0b010
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 8), 48);   // 0b00110000
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 3), 3);    // 0b011
    println!("offset: {}", offset);
    assert_eq!(poll_bits_increase_offset(&data, &mut offset, 5), 0);    // 0b00000
    println!("offset: {}", offset);
    // check offset
    assert_eq!(offset, 64);
}

trait HierarchicalVersion {
    fn get_version_sum(&self) -> u32;
}

trait PacketValue {
    fn get_value(&self) -> u64;
}

enum Op {
    Sum,
    Product,
    Minimum,
    Maximum,
    GreaterThan,
    LessThan,
    Equal,
}

impl TryFrom<u8> for Op {
    type Error = std::io::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Op::Sum),
            1 => Ok(Op::Product),
            2 => Ok(Op::Minimum),
            3 => Ok(Op::Maximum),
            5 => Ok(Op::GreaterThan),
            6 => Ok(Op::LessThan),
            7 => Ok(Op::Equal),
            // invalid type
            _ => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "invalid type for Operation")),
        }       
    }
}

impl Op {
    pub fn apply(&self, values: &[u64]) -> u64 {
        match self {
            Self::Sum           => values.iter().sum(),
            Self::Product       => values.iter().product(),
            Self::Minimum       => values.iter().min().map(|n| *n).unwrap_or(0),
            Self::Maximum       => values.iter().max().map(|n| *n).unwrap_or(0),
            Self::GreaterThan   => if values[0] > values[1] { 1 } else { 0 },
            Self::LessThan      => if values[0] < values[1] { 1 } else { 0 },
            Self::Equal         => if values[0] == values[1] { 1 } else { 0 },
        }
    }
}

enum PacketType {
    Literal(u64),
    Operator(Op, Vec<Packet>),
}

impl HierarchicalVersion for PacketType {
    fn get_version_sum(&self) -> u32 {
        match self {
            // no version defined within literal value
            Self::Literal(_) => 0,
            // sum up versions of all subpackets
            Self::Operator(_, subpackets) => subpackets.iter().map(|p| p.get_version_sum()).sum(),
        }
    }
}

impl PacketValue for PacketType {
    fn get_value(&self) -> u64 {
        match self {
            Self::Literal(n) => *n,
            Self::Operator(op, packets) => {
                let vals: Vec<u64> = packets.iter().map(|p| p.get_value()).collect();
                op.apply(&vals)
            },
        }
    }
}

impl TryFrom<(&mut usize, &[u8])> for PacketType {
    type Error = std::io::Error;

    fn try_from(value: (&mut usize, &[u8])) -> Result<Self, Self::Error> {
        let (offset, data) = value;
        let ptype = poll_bits_increase_offset(data, offset, 3);
        if ptype == 4 {
            // literal value
            let mut cont_bit: u8 = 1;
            let mut value: u64 = 0;
            while cont_bit != 0 {
                // update continuation bit
                cont_bit = poll_bits_increase_offset(data, offset, 1);
                // poll next 4 bits and add to value
                value = value << 4;
                value += poll_bits_increase_offset(data, offset, 4) as u64;
            }
            Ok(Self::Literal(value))
        } else {
            // operator value with subpackets
            let op = Op::try_from(ptype)?;
            let length_type_id = poll_bits_increase_offset(data, offset, 1);
            let mut subpackets = Vec::new();
            if length_type_id == 0 {
                let mut total_length_subpackets: u16 = poll_bits_increase_offset(data, offset, 8) as u16;
                total_length_subpackets = total_length_subpackets << 7;
                total_length_subpackets += poll_bits_increase_offset(data, offset, 7) as u16;

                // read subpackets until we reach the total length
                let initial_offset: usize = *offset;
                while (*offset - initial_offset) < (total_length_subpackets as usize) {
                    let mut sub_offset: usize = *offset;
                    let sub: Packet = Packet::try_from((&mut sub_offset, data))?;
                    subpackets.push(sub);
                    *offset = sub_offset;
                }
            } else {
                let mut num_subpackets: u16 = poll_bits_increase_offset(data, offset, 8) as u16;
                num_subpackets = num_subpackets << 3;
                num_subpackets += poll_bits_increase_offset(data, offset, 3) as u16;

                // read specified number of subpackets
                for _ in 0..num_subpackets {
                    let mut sub_offset: usize = *offset;
                    let sub: Packet = Packet::try_from((&mut sub_offset, data))?;
                    subpackets.push(sub);
                    *offset = sub_offset;
                }
            }
            Ok(Self::Operator(op, subpackets))
        }
    }
}

struct Packet {
    version: u8,
    packet_type: PacketType,
}

impl HierarchicalVersion for Packet {
    fn get_version_sum(&self) -> u32 {
        return self.version as u32 + self.packet_type.get_version_sum();
    }
}

impl PacketValue for Packet {
    fn get_value(&self) -> u64 {
        return self.packet_type.get_value();
    }
}

impl TryFrom<(&mut usize, &[u8])> for Packet {
    type Error = std::io::Error;

    fn try_from(value: (&mut usize, &[u8])) -> Result<Self, Self::Error> {
        let (offset, data) = value;
        let version = poll_bits_increase_offset(data, offset, 3);
        let packet_type = PacketType::try_from((offset, data))?;

        Ok(Self {
            version,
            packet_type,
        })
    }
}

#[cfg(test)]
fn packet_test(input: String) -> Packet {
    let len = input.len();
    println!("input ({}): {}", input.len(), &input);
    let data = read_hex_data(input).expect("unable to read hex data from input");
    println!("hex data ({}): {}", data.len(), data.iter().map(|b| format!("{:08b}", b)).collect::<Vec<String>>().join(", "));
    let mut offset = data.len() * 8 - len * 4;
    let packet = Packet::try_from((&mut offset, data.as_slice())).expect("unable to convert data to packet");
    return packet;
}

#[cfg(test)]
fn test_packet_version(input: &str) -> u32 {
    packet_test(input.to_string()).get_version_sum()
}

#[test]
fn test_packet_version1() {
    assert_eq!(test_packet_version("8A004A801A8002F478"), 16);
}

#[test]
fn test_packet_version2() {
    assert_eq!(test_packet_version("620080001611562C8802118E34"), 12);
}

#[test]
fn test_packet_version3() {
    assert_eq!(test_packet_version("C0015000016115A2E0802F182340"), 23);
}

#[test]
fn test_packet_version4() {
    assert_eq!(test_packet_version("A0016C880162017C3686B18A3D4780"), 31);
}

#[cfg(test)]
fn test_packet_value(input: &str) -> u64 {
    packet_test(input.to_string()).get_value()
}

#[test]
fn test_packet_value1() {
    assert_eq!(test_packet_value("C200B40A82"), 3);
}

#[test]
fn test_packet_value2() {
    assert_eq!(test_packet_value("04005AC33890"), 54);
}

#[test]
fn test_packet_value3() {
    assert_eq!(test_packet_value("880086C3E88112"), 7);
}

#[test]
fn test_packet_value4() {
    assert_eq!(test_packet_value("CE00C43D881120"), 9);
}

#[test]
fn test_packet_value5() {
    assert_eq!(test_packet_value("D8005AC2A8F0"), 1);
}

#[test]
fn test_packet_value6() {
    assert_eq!(test_packet_value("F600BC2D8F"), 0);
}

#[test]
fn test_packet_value7() {
    assert_eq!(test_packet_value("9C005AC2F8F0"), 0);
}

#[test]
fn test_packet_value8() {
    assert_eq!(test_packet_value("9C0141080250320F1802104A08"), 1);
}

fn main() -> std::io::Result<()> {
    let filename = std::env::args().skip(1).next().unwrap_or("input".to_string());
    let input = std::fs::read_to_string(filename)?.lines().next().unwrap().to_string();
    let len = input.len();
    let input_data = read_hex_data(input)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, format!("could not parse input as hex number: {}", e)))?;

    // calculate initial offset from bitlength of total data input - bitlength of input data
    // this results in the offset starting at the byte padding overhang
    let mut offset: usize = input_data.len() * 8 - len * 4;
    let top_packet = Packet::try_from((&mut offset, input_data.as_slice()))?;
    println!("a. Combined sum of all version numbers: {}", top_packet.get_version_sum());
    println!("b. Evaluated expression of BITS transmission: {}", top_packet.get_value());

    Ok(())
}

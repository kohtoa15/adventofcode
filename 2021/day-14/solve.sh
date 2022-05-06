echo "# build"
cabal build
echo ""

echo "# run test"
cat example.txt | ./dist-newstyle/build/*/*/day14-*/x/day14/build/day14/day14
echo ""

echo "# run"
cat input.txt | ./dist-newstyle/build/*/*/day14-*/x/day14/build/day14/day14
echo ""

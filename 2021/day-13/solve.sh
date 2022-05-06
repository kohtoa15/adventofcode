echo "# build"
cabal build
echo ""

echo "# run test"
cat example.txt | ./dist-newstyle/build/*/*/day13-*/x/day13/build/day13/day13
echo ""

echo "# run"
cat input.txt | ./dist-newstyle/build/*/*/day13-*/x/day13/build/day13/day13
echo ""

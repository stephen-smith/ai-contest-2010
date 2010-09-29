prog=${1:-./MyBot}

for bot in Random Bully Prospector Dual Rage; do
	botbin=example_bots/${bot}Bot
	ghc --make -O2 -v0 $botbin
	for map in maps/map*.txt; do
		java -jar tools/PlayGame.jar "$map" 1000 200 PlayGame.log \
			"$prog" "$botbin" > last.out 2> last.err
		echo "$map" > last.map
		grep -v '^Turn' last.err
		grep -q 'timed out' last.err && break
	done > results
	printf 'Against %sBot\n' "$bot"
	sort results | uniq -c
	grep -q 'timed out' results && break
done
! grep -q 'timed out' results

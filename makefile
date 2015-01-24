# meals.pl testing.  -- rgr, 24-Jan-15.

test:	test-day1 test-day2 test-day3

test-day1:
	./meals.pl day1.text > $@.tmp 2>/dev/null
	cmp $@.tbl $@.tmp
	rm -f $@.tmp
test-day2:
	./meals.pl day2.text > $@.tmp 2>/dev/null
	cmp $@.tbl $@.tmp
	rm -f $@.tmp
test-day3:
	./meals.pl --det day3.text > $@.tmp 2>/dev/null
	cmp $@.text $@.tmp
	rm -f $@.tmp

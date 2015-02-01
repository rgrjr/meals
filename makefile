# meals.pl testing.  -- rgr, 24-Jan-15.

test:	test-day1 test-day2 test-day3 test-day4

test-day1:
	./meals.pl day1.text > $@.tmp
	cmp $@.tbl $@.tmp
	rm -f $@.tmp
test-day2:
	./meals.pl day2.text > $@.tmp
	cmp $@.tbl $@.tmp
	rm -f $@.tmp
test-day3:
	./meals.pl --det day3.text > $@.tmp 2>&1
	cmp $@.text $@.tmp
	rm -f $@.tmp
test-day4:
	./meals.pl --det day4.text > $@.tmp
	cmp $@.text $@.tmp
	rm -f $@.tmp

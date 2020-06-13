# meals.pl testing.  -- rgr, 24-Jan-15.

test:	test-null test-show1 test-day1 test-day2 test-day3 test-day4 test-plot

test-null:
	./meals.pl --recipe-file recipes.text --det=item /dev/null > $@.tmp
	cmp /dev/null $@.tmp
	rm -f $@.tmp
# Test various levels of detail.
TEST-DAY1 = --recipe-file day1-recipes.text day1.text
test-day1:
	./meals.pl ${TEST-DAY1} --det=item > $@.tmp
	cmp $@.item.tbl $@.tmp
	./meals.pl ${TEST-DAY1} > $@.tmp
	cmp $@.tbl $@.tmp
	./meals.pl ${TEST-DAY1} --det=meal > $@.tmp
	cmp $@.tbl $@.tmp
	./meals.pl ${TEST-DAY1} --det=day > $@.tmp
	cmp $@.day.tbl $@.tmp
	./meals.pl ${TEST-DAY1} --det=none --file > $@.tmp
	cmp $@.file.tbl $@.tmp
	rm -f $@.tmp
test-day2:
	./meals.pl day2.text > $@.tmp
	cmp $@.tbl $@.tmp
	rm -f $@.tmp
test-day3:
	./meals.pl --file --det=item day3.text > $@.tmp 2>&1
	cmp $@.text $@.tmp
	rm -f $@.tmp
test-plot:
	./meals.pl --recipe-file test-recipes.text day2.text day3.text \
	    --plot-cho $@.cho.tmp --plot-cal $@.cal.tmp > $@.tmp 2>/dev/null
	file $@.cal.tmp | grep -q 'PNG image data'
	file $@.cho.tmp | grep -q 'PNG image data'
	cmp /dev/null $@.tmp
	rm -f $@.tmp $@.cho.tmp $@.cal.tmp
test-day4:
	./meals.pl --recipe-file recipes.text --file \
		--det=item day4.text > $@.tmp
	cmp $@.text $@.tmp
	rm -f $@.tmp
test-show1:
	./meals.pl --recipe-file recipes.text --show "Jan's pancakes" > $@.tmp
	cmp $@.text $@.tmp
	rm -f $@.tmp

tags:
	find . -name '*.p[lm]' | etags -

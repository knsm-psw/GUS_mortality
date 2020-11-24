#!/usr/bin/perl
%AA = (##
'A4'  => '00-04',
'A9'  => '05-09',
'A14' => '10-14',
'A19' => '15-19',
'A24' => '20-24',
'A29' => '25-29',
'A34' => '30-34',
'A39' => '35-39',
'A44' => '40-44',
'A49' => '45-49',
'A54' => '50-54',
'A59' => '55-59',
'A64' => '60-64',
'A69' => '65-69',
'A74' => '70-74',
'A79' => '75-79',
'A84' => '80-84',
'A89' => '85-89',
'A99' => '90-99',
'age' => 'age',
'T'   => 'OGÓŁEM' );

###

$Y = 2015;

print "year;sex;week;date;age;geo;value\n";
while (<>) {
### year;sex;week;date;age;geo;value
  chomp();
  ($year, $sex, $week, $date, $age, $geo, $value) = split /;/, $_;
  if ($year >= $Y )  {
    $age= $AA{$age};
    $week=~ s/T//;
    print "$year;$sex;$week;$date;$age;$geo;$value\n";
  }
  $lines++;
}
print STDERR "$lines extracted!\n";

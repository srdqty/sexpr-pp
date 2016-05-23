for file in test_cases/*.in
  do
    echo "********** Testing $file **********"
    ./read_test < $file | diff - ${file%in}out
    if [ $? -eq 0 ]; then echo "Success"; else echo "Failure"; fi
  done

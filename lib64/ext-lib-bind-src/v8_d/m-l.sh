ll="libicui18n"
cd $ll
ar rvs ../$ll.a *.o
cd ..

ll="libicudata" 
cd $ll
ar rvs ../$ll.a *.o
cd ..

ll="libicuuc"
cd $ll
ar rvs ../$ll.a *.o
cd ..

ll="libv8_base"
cd $ll
ar rvs ../$ll.a *.o
cd ..

ll="libv8_nosnapshot"
cd $ll
ar rvs ../$ll.a *.o
cd ..

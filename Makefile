F90=ifort
FCFLAGS=-O3
LDFLAGS=

TARGET= PAR_TRACK.exe
OBJECT= mod_kinds.o mod_particle_tracking.o main.o

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^ $(LDFLAGS)

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm -f *.mod
	rm -f *.o
	rm PAR_TRACK.exe

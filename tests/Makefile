# Compiler
CXX = g++

# Compiler flags
CXXFLAGS = -Wall -g

# Include path
INCLUDE_PATH = ../include

# Target executable
TARGET = basic 

# Source files
SRCS = basic.cpp

# Header files
HDRS = $(wildcard $(INCLUDE_PATH)/isle/*.h)

# Object files
OBJS = $(SRCS:.cpp=.o)

# Default target
all: $(TARGET)

# Link object files to create the executable
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS)

# Compile source files to object files
%.o: %.cpp $(HDRS)
	$(CXX) $(CXXFLAGS) -I$(INCLUDE_PATH) -c $< -o $@

# Clean up build files
clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: all clean
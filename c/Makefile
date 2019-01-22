TARGET_NAME ?= a.out

BUILD_DIR ?= ./build
SRC_DIRS ?= .

SRCS := $(shell find $(SRC_DIRS) -name *.c)
OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)
DEPS := $(OBJS:.o=.d)

INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

CFLAGS  := $(CFLAGS) -fPIC -MMD -MP
LDFLAGS := $(LDFLAGS) -shared -fPIC 

$(BUILD_DIR)/$(TARGET_NAME): $(OBJS)
	$(CC) $(OBJS) -o $@ $(LDFLAGS)

# C source
$(BUILD_DIR)/%.c.o: %.c
	$(MKDIR_P) $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean

clean:
	$(RM) -r $(BUILD_DIR)

-include $(DEPS)

MKDIR_P ?= mkdir -p

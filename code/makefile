# Makefile for the entire project
# Requires GNU Make

# Main project pipeline

all: visualization

.PHONY: all raw derived analysis

raw:
	$(MAKE) -C simulation/code 

derived:
	$(MAKE) -C analysis/code 

analysis: derived
	$(MAKE) -C visualization/code
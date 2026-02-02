# Makefile for Westwood et al. (2022) Replication

.PHONY: all slides open figures clean

all: figures slides

# Render Marp slides to PDF
slides: westwood-replication-talk.pdf

westwood-replication-talk.pdf: westwood-replication-talk.md results/*.png
	marp --no-stdin --pdf --allow-local-files $< -o $@

# Open the rendered PDF
open: westwood-replication-talk.pdf
	open $<

# Generate figures from R code
figures:
	cd code && Rscript run_core.R

# Convert PDFs to PNGs for Marp
pngs: results/*.pdf
	for f in results/*.pdf; do convert -density 150 "$$f" "$${f%.pdf}.png"; done

# Clean generated files
clean:
	rm -f westwood-replication-talk.pdf results/*.png

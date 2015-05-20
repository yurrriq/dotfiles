config.fish: ~/.config/fish/config.fish
	rsync -avz $< .

clean:
	rm -f config.fish

plan:
	@rm -rf -- "$(HOME)/.plan"
	@cp "$(HOME)/.dotfiles/plan" "$(HOME)/.plan"

.PHONY: plan
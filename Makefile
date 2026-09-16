build:
	@docker compose build

up: 
	@docker compose up -d

down:
	@docker compose down -v

all: up

clean:
	@docker system prune -af

re: 
	@make --no-print-directory down && make --no-print-directory up


logs:
	@docker compose logs builder -f
all:

start: IMAGE := mysql:5.7
start: _var
	docker run -d --rm \
		--name crawler-app-db \
		--cidfile docker.cid \
		-e MYSQL_ROOT_PASSWORD=hogehoge \
		-v `pwd`/var:/var/lib/mysql:rw \
		-p 3306:3306 \
		$(IMAGE)

_var:
	mkdir -p $@

stop:
	-[ -f docker.cid ] && docker stop $$(cat docker.cid)
	rm -f docker.cid

clean:
	rm -rf var

.PHONY: all start stop clean

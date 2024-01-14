let x = 5;

let add = fn(x) {
	fn(y) { x + y; };
};

print(add(5)(5));
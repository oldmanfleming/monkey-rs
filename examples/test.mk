let counter = fn(x) {
  if (x > 1000) {
    return true;
  } else {
    let foobar = 9999;
    counter(x + 1);
  }
};

let run = fn(x) {
	if (x > 1000000000000) {
		return true;
	} else {
		let foobar = 8888;
		counter(0);
		run(x + 1);
	}
};

run(0);
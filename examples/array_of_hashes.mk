let people = [{"name": "Alice", "age": 24}, {"name": "Anna", "age": 28}];

print(people[0]["name"]);

print(people[1]["name"]);

print(people[1]["age"] + people[0]["age"]);

let getName = fn(person) { person["name"]; };

print(getName(people[0]));

print(getName(people[1]));
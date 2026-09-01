module type Classifier = module type of struct
  let classify value = value
end

module For_testing = struct
  let classify value = value
end

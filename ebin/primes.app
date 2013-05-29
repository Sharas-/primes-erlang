{application, primes,
 [
  {description, "Lists prime numbers from a range of integers"},
  {modules, ['primes', 'prime_tester']},
  {applications, [stdlib, kernel]},
  {env, []}
 ]
}.

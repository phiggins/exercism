module SpaceAge where
  data Planet = Earth
              | Jupiter
              | Mars
              | Mercury
              | Neptune
              | Saturn
              | Uranus
              | Venus

  ageOn :: Planet -> Integer -> Double
  ageOn Earth n     = fromIntegral n / (365.25 * 86400)
  ageOn Jupiter n   = (ageOn Earth n) / 11.862615
  ageOn Mars n      = (ageOn Earth n) / 1.8808158
  ageOn Mercury n   = (ageOn Earth n) / 0.2408467
  ageOn Neptune n   = (ageOn Earth n) / 164.79132
  ageOn Saturn n    = (ageOn Earth n) / 29.447498
  ageOn Uranus n    = (ageOn Earth n) / 84.016846
  ageOn Venus n     = (ageOn Earth n) / 0.61519726

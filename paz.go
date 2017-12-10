package main

//Paz

import (
  "fmt"
  "math"
)

const (
  N = 9
  K = 5
)

var epsilon_hat_t_star_squared = []float64{
  -1, // Ignore. Just use to shift everything to 1 index.

  0.00008227,
  0.00006037,
  0.00006514,
  0.00007383,
  0.00007987,
  0.00008616,
  0.00009315,
  0.00009193,
  0.00007576,
}

var h = []float64{
  -1,  // Ignore. Just use to shift everything to 1 index.

  0.0059989,
  0.0014179,
  0.00021526,
  0.00092101,
  0.00532979,
  0.00011901,
  0.00668712,
  0.00356766,
  0.00098876,
}

var gamma_hat = [][]float64{
  {}, // Ignore. Just use to shift everything to 1 index. 

  {3.02E-06, 0.20104818, 0.76824534},
  {1.51E-06, 0.26306283, 0.78917878},
  {1.47E-06, 0.28297351, 0.70827203},
  {0.0000146, 0.15079103, 1.0258335},
  {0.0000316, 0.11034539, 1.2668431},
  {0.00003109, 0.10288763, 1.2327034},
  {0.00001534, 0.15617694, 0.98304705},
  {0.00001745, 0.15619251, 1.0085176},
  {0.00001985, 0.12705008, 1.1192125},
}

var epsilon_hat = [][]float64{
  // There have to be N rows in this matrix and K columns.
  {},

  {-1,  0.00123472, -0.00396937, 0.00119579, -0.0025328, -0.00097436},
  {-1,  0.00258164, -0.01113955, -0.01940827, -0.0286924, 0.02065209},
  {-1,  0.00474626, 0.00113553, -0.00885011, 0.00735987, -0.00721973},
  {-1,  -0.00587587, 0.00696358, 0.00551218, -0.00054735, -0.00538458},
  {-1,  -0.00018127, 0.00959092, -0.00038023, -0.00483037, -0.00103533},
  {-1,  0.02492378, 0.02561009, 0.00923541, 0.00728951, -0.00298362},
  {-1,  -0.00417943, -0.00122376, -0.00685361, -0.00285596, 0.00164085},
  {-1,  -0.0046094, 0.0019101, 0.00798739, -0.00164692, 0.0051483},
  {-1,  0.00075892, 0.00289005, -0.00108842, 0.00510889, 0.00144077},
}

func E(i, k int) float64 {
  sum := 0.0
  for j := 1; j <= k - 1; j++ {
    sum += math.Pow(gamma_hat[i][1] + gamma_hat[i][2], float64(j))
  }
  term1 := gamma_hat[i][0] * sum
  term2 := math.Pow(gamma_hat[i][1] + gamma_hat[i][2], float64(k - 1)) * gamma_hat[i][1] * h[i]
  term3 := math.Pow(gamma_hat[i][1] + gamma_hat[i][2], float64(k - 1)) * gamma_hat[i][2] * epsilon_hat_t_star_squared[i]
  return  term1 + term2 + term3
}

func M_hat(t int) float64 {
  outer_sum := 0.0
  for i := 1; i <= N; i++ {
     // Find numerator of big sum.
     top_sum := 0.0;
     for j := 1; j <= N; j++ {  // TODO: What is the lowest index? The top sum doesn't specify so I assumed it's 1: https://www.evernote.com/l/AoGOTxcbhHlA6JNimj-_oxG3IDrvNY63UzY
        top_sum += epsilon_hat[j][t]
     }
     numerator := math.Pow(N * epsilon_hat[i][t] - top_sum, 2)
  
     // Find denominator of big sum.
     bottom_sum := 0.0
     for j := 1; j <= N; j++ {
       bottom_sum += E(j, t)
     }
     denominator := N * (N - 2) * E(i, t) + bottom_sum
  
     outer_sum += numerator / denominator
  }
  return outer_sum / float64(N - 1)
}

func phi(t1, t2 int) float64 {
  sum := 0.0
  for t := t1; t <= t2; t++ {
    sum += (N - 1) * M_hat(t)
  }
  return sum
}

func cav(t1, t2 int) float64 {
  sum := 0.0
  for t := t1; t <= t2; t++ {
    sum += M_hat(t)
  }
  return sum - float64(t2 - t1 + 1);
}

func main() {
  c := cav(1, K)
  fmt.Printf("cav = %f\n", c)

  p := phi(1, K)
  fmt.Printf("phi = %f\n", p)
}



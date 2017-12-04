package main

//Bogota

import (
  "fmt"
  "math"
)

const (
  N = 13
  K = 5
)

var epsilon_hat_t_star_squared = []float64{
  -1, // Ignore. Just use to shift everything to 1 index.

  0.00033068,
  0.00032867,
  0.00014628,
  0.0001425,
  0.00013594,
  0.00013389,
  0.00017054,
  0.00018473,
  0.00018812,
  0.00018654,
  0.00012433,
  0.00008134,
  0.00008078,
}

var h = []float64{
  -1,  // Ignore. Just use to shift everything to 1 index.

  0.00420707,
  0.00636394,
  0.00202532,
  0.00811886,
  0.00228647,
  0.00752678,
  0.0079782,
  0.00238573,
  0.00181124,
  0.00159903,
  0.00233375,
  0.00473748,
  0.00756805,
}

var gamma_hat = [][]float64{
  {}, // Ignore. Just use to shift everything to 1 index. 

  {0.00003446, 0.62399931, 0.2544311},
  {0.00003628, 0.57727404, 0.26340729},
  {0.00003242, 0.27347341, 0.47626483},
  {0.00005584, 0.29172289, 0.28108623},
  {0.00005432, 0.26663837, 0.29088093},
  {0.0000472, 0.20729118, 0.38482037},
  {0.00001617, 0.18941574, 0.87420102},
  {4.79E-07, 0.28872858, 0.70324138},
  {3.05E-06, 0.27545246, 0.73159016},
  {1.00E-05, 0.25880672, 0.7819547},
  {0.00002257, 0.21460269, 0.95838078},
  {2.35E-06, 0.18897947, 0.84832352},
  {1.21E-06, 0.18543894, 0.83586429},
}

var epsilon_hat = [][]float64{
  // There have to be N rows in this matrix and K columns.
  {},

  {-1,  0.03884043, -0.00019583, 0.01821263, 0.01774671, -0.00720784},
  {-1,  -0.00745248, 0.00310902, -0.00135684, 0.00209083, -0.00053282},
  {-1,  0.01210912, 0.00611933, 0.00104543, -0.0026784, -0.01037796},
  {-1,  0.00886543, -0.00321336, 0.00554449, -0.00414219, 0.01098214},
  {-1,  0.00483459, 0.00057268, 0.00001635, 0.00290101, -0.00082736},
  {-1,  0.00781664, -0.03511741, 0.00256151, -0.02173027, -0.01392378},
  {-1,  -0.02697139, -0.02975366, -0.0135537, 0.01052388, 0.03946977},
  {-1,  0.02189712, 0.0077918, -0.00801664, 0.03040891, 0.01524212},
  {-1,  -0.00807494, 0.00377769, -0.0049692, -0.00647268, 0.01822953},
  {-1,  0.00374495, 0.00879661, -0.00066377, 0.00999523, -0.00627229},
  {-1,  -0.00278311, 0.00381476, -0.00046265, 0.00224628, 0.00459889},
  {-1,  0.00041789, 0.00729119, -0.00220175, -0.00380651, -0.00536472},
  {-1,  0.00390625, 0.00375535, -0.00613875, 0.00125038, 0.01935462},
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

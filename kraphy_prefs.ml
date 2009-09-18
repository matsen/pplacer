type kraphy_prefs = 
  {
    verbose: bool ref;
    n_shuffles: int ref;
    out_fname: string ref;
    histo: bool ref;
    p_plot: bool ref;
    p_exp: float ref;
    dropdown: float ref;
  }

let verbose    p = !(p.verbose)
let out_fname  p = !(p.out_fname)
let n_shuffles p = !(p.n_shuffles)
let histo      p = !(p.histo)
let p_plot     p = !(p.p_plot)
let p_exp      p = !(p.p_exp)
let dropdown   p = !(p.dropdown)

// Copy std lib to
fn main() {
    println!("cargo::rerun-if-changed=../ecsl_std");

    let home = homedir::my_home().unwrap().unwrap();

    let mut dot_ecsl = home.clone();
    dot_ecsl.push(".ecsl");

    let mut ecsl_std = dot_ecsl.clone();
    ecsl_std.push("ecsl_std");

    _ = std::fs::create_dir_all(&dot_ecsl);
    _ = std::fs::remove_dir_all(&ecsl_std);
    _ = copy_dir::copy_dir("../ecsl_std", &ecsl_std);
}

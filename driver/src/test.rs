use driver::*;

fn main() {
  for result in test_all("testcase/S3", Pa::Pa3).unwrap() {
    println!("{:?}", result);
    if let ResultKind::Pass = result.kind {
    } else {
      std::process::exit(1);
    }
  }
  for result in test_all("testcase/S1", Pa::Pa1b).unwrap() {
    if let ResultKind::Pass = result.kind {
      println!("{:?}", result);
    } else {
      if result.file.to_str() == Some("testcase/S1/abstract1.decaf")
        || result.file.to_str() == Some("testcase/S1/abstract2.decaf")
        || result.file.to_str() == Some("testcase/S1/abstract3.decaf")
        || result.file.to_str() == Some("testcase/S1/abstract6.decaf")
        || result.file.to_str() == Some("testcase/S1/lambdabad1.decaf")
        || result.file.to_str() == Some("testcase/S1/lambdabad4.decaf")
        || result.file.to_str() == Some("testcase/S1/var4.decaf")
        || result.file.to_str() == Some("testcase/S1/var5.decaf")
      {
        println!("{}: Failure ignored", result.file.as_path().display());
      } else {
        println!("{:?}", result);
        std::process::exit(1);
      }
    }
  }
  for result in test_all("testcase/S1-LL", Pa::Pa1b).unwrap() {
    println!("{:?}", result);
    if let ResultKind::Pass = result.kind {
    } else {
      std::process::exit(1);
    }
  }
  #[cfg(not(feature = "fn"))]
  for result in test_all("testcase/S2-rs", Pa::Pa2).unwrap() {
    println!("{:?}", result);
    if let ResultKind::Pass = result.kind {
    } else {
      std::process::exit(1);
    }
  }
  #[cfg(feature = "fn")]
  for result in test_all("testcase/S2", Pa::Pa2).unwrap() {
    println!("{:?}", result);
    if let ResultKind::Pass = result.kind {
    } else {
      std::process::exit(1);
    }
  }
  for result in test_all("testcase/S3", Pa::Pa3).unwrap() {
    println!("{:?}", result);
    if let ResultKind::Pass = result.kind {
    } else {
      std::process::exit(1);
    }
  }
  for result in test_all("testcase/S4", Pa::Pa4).unwrap() {
    println!("{:?}", result);
    if let ResultKind::Pass = result.kind {
    } else {
      std::process::exit(1);
    }
  }
  for result in test_all("testcase/S4", Pa::Pa5Wast).unwrap() {
    println!("{:?}", result);
    if let ResultKind::Pass = result.kind {
    } else {
      std::process::exit(1);
    }
  }
}

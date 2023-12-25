open Core

let () =
  printf "Hello! This is the Monkey programming language!\n";
  printf "Feel free to type in commands\n";
  Out_channel.flush Out_channel.stdout;
  Repl.start In_channel.stdin Out_channel.stdout

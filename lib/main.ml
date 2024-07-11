open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let print_game (game : Game.t) =
    let n = Game.Game_kind.board_length game.game_kind in
    let lists_to_print =
      List.init n ~f:(fun row ->
        List.init n ~f:(fun col ->
          if Map.existsi game.board ~f:(fun ~key ~data ->
               ignore data;
               Game.Position.equal
                 key
                 { Game.Position.row; Game.Position.column = col })
          then
            Game.Piece.to_string
              (Map.find_exn
                 game.board
                 { Game.Position.row; Game.Position.column = col })
          else " "))
    in
    let rows_as_strings =
      List.map lists_to_print ~f:(fun row_list ->
        String.concat ~sep:" | " row_list)
    in
    List.iteri rows_as_strings ~f:(fun row_num row ->
      Core.print_endline row;
      if row_num < n - 1 then (if n=3 then Core.print_endline "---------" else Core.print_endline "------------------------------------------------------"))
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
let get_close_available_moves (game : Game.t) : Game.Position.t list =;;

  let available_moves (game : Game.t) : Game.Position.t list =
    let n = Game.Game_kind.board_length game.game_kind in
    let all_board_positions =
      List.concat
        (List.init n ~f:(fun row ->
           List.init n ~f:(fun col ->
             { Game.Position.row; Game.Position.column = col })))
    in
    List.filter all_board_positions ~f:(fun position ->
      not
        (Map.existsi game.board ~f:(fun ~key ~data ->
           ignore data;
           Game.Position.equal position key)))
  ;;

  module Direction = struct
    type t =
      | Right
      | Bottom_left
      | Bottom_middle
      | Bottom_right
  end

  let neighbors_in_direction
    { Game.Position.row = origin_row; Game.Position.column = origin_col }
    (direction : Direction.t)
    how_many
    =
    match direction with
    | Right ->
      List.init how_many ~f:(fun n ->
        { Game.Position.row = origin_row
        ; Game.Position.column = origin_col + n + 1
        })
    | Bottom_left ->
      List.init how_many ~f:(fun n ->
        { Game.Position.row = origin_row + n + 1
        ; Game.Position.column = origin_col - n - 1
        })
    | Bottom_middle ->
      List.init how_many ~f:(fun n ->
        { Game.Position.row = origin_row + n + 1
        ; Game.Position.column = origin_col
        })
    | Bottom_right ->
      List.init how_many ~f:(fun n ->
        { Game.Position.row = origin_row + n + 1
        ; Game.Position.column = origin_col + n + 1
        })
  ;;

  let find_win position (game : Game.t) ~piece =
    let goal_count = Game.Game_kind.win_length game.game_kind in
    let direction_list =
      [ Direction.Right; Bottom_left; Bottom_middle; Bottom_right ]
    in
    let is_a_solution_in_that_direction_list =
      List.filter_map direction_list ~f:(fun dir ->
        let neighbors_list =
          neighbors_in_direction position dir (goal_count - 1)
        in
        if List.exists neighbors_list ~f:(fun neighbor ->
             not
               (Map.existsi game.board ~f:(fun ~key ~data ->
                  ignore data;
                  Game.Position.equal key neighbor)))
        then None
        else if List.exists neighbors_list ~f:(fun nbr ->
                  let nbr_piece = Map.find_exn game.board nbr in
                  not (Game.Piece.equal piece nbr_piece))
        then None
        else Some piece)
    in
    if List.length is_a_solution_in_that_direction_list = 0
    then None
    else Some (List.hd_exn is_a_solution_in_that_direction_list)
  ;;

  let search_for_solution_from_position position (game : Game.t) =
    (* if return None then no win from that position, else returns piece that
       won *)
    let is_occupied =
      Map.existsi game.board ~f:(fun ~key ~data ->
        ignore data;
        Game.Position.equal position key)
    in
    match is_occupied with
    | false -> None
    | true ->
      let piece = Map.find_exn game.board position in
      (match piece with
       | X -> find_win position game ~piece:Game.Piece.X
       | O -> find_win position game ~piece:Game.Piece.O)
  ;;

  (* Exercise 2 *)
  let no_available_spaces (game:Game.t) =
    Map.length game.board = Game.Game_kind.board_length game.game_kind * Game.Game_kind.board_length game.game_kind
  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    let n = Game.Game_kind.board_length game.game_kind in
    let all_board_positions =
      List.concat
        (List.init n ~f:(fun row ->
           List.init n ~f:(fun col ->
             { Game.Position.row; Game.Position.column = col })))
    in
    let final =
      List.filter_map all_board_positions ~f:(fun pos ->
        search_for_solution_from_position pos game)
    in
    if List.length final = 0
    then( if (no_available_spaces game) then Game.Evaluation.Game_over { winner = None } else Game.Evaluation.Game_continues)
    else Game.Evaluation.Game_over { winner = Some (List.hd_exn final) }
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let all_possible_placements = available_moves game in
    let winning_moves =
      List.filter all_possible_placements ~f:(fun position ->
        let new_game = Game.empty game.game_kind in
        let res =
          Map.fold
            game.board
            ~init:new_game
            ~f:(fun ~key ~data building_game ->
              place_piece building_game ~piece:data ~position:key)
        in
        let final_new_game = place_piece res ~piece:me ~position in
        match evaluate final_new_game with
        | Game.Evaluation.Game_continues -> false
        | _ -> true)
    in
    winning_moves
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    winning_moves ~me:(Game.Piece.flip me) game
  ;;

  let available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    =
    let opponents_winning_moves = losing_moves ~me game in
    match List.length opponents_winning_moves with
    | 0 -> available_moves game
    | 1 -> opponents_winning_moves
    | _ -> []
  ;;

  let get_next_game_states game ~piece =
    let all_available_moves = available_moves game in
    List.map all_available_moves ~f:(fun move_to_make ->
      place_piece game ~piece ~position:move_to_make)
  ;;

  let _get_better_neighbors { Game.Position.row; column = col } =
    [ { Game.Position.row; column = col + 1 }
    ; { Game.Position.row; column = col + 2 }
    ; { Game.Position.row = row + 1; column = col - 1 }
    ; { Game.Position.row = row + 1; column = col }
    ; { Game.Position.row = row + 1; column = col + 1 }
    ; { Game.Position.row = row + 2; column = col - 2 }
    ; { Game.Position.row = row - 2; column = col }
    ; { Game.Position.row = row + 2; column = col + 2 }
    ]
  ;;

  let get_neighbors { Game.Position.row; column = col } =
    [ { Game.Position.row = row - 2; column = col - 2 }
    ; { Game.Position.row = row - 2; column = col }
    ; { Game.Position.row = row - 2; column = col + 2 }
    ; { Game.Position.row = row - 1; column = col - 1 }
    ; { Game.Position.row = row - 1; column = col }
    ; { Game.Position.row = row - 1; column = col + 1 }
    ; { Game.Position.row; column = col - 2 }
    ; { Game.Position.row; column = col - 1 }
    ; { Game.Position.row; column = col + 1 }
    ; { Game.Position.row; column = col + 2 }
    ; { Game.Position.row = row + 1; column = col - 1 }
    ; { Game.Position.row = row + 1; column = col }
    ; { Game.Position.row = row + 1; column = col + 1 }
    ; { Game.Position.row = row + 2; column = col - 2 }
    ; { Game.Position.row = row - 2; column = col }
    ; { Game.Position.row = row + 2; column = col + 2 }
    ]
  ;;

  let evaluate_how_many_consecutive_pieces_on_current_board
    (game : Game.t)
    ~piece_to_eval
    =
    (* returns a number that is bigger if the current piece is winning and
       smaller if the other piece is winning*)
    let n = Game.Game_kind.board_length game.game_kind in
    let all_board_positions =
      List.concat
        (List.init n ~f:(fun row ->
           List.init n ~f:(fun col ->
             { Game.Position.row; Game.Position.column = col })))
    in
    List.fold all_board_positions ~init:0 ~f:(fun acc a_position ->
      if not
           (Map.existsi game.board ~f:(fun ~key ~data ->
              ignore data;
              (* not a valid neighbor and contributes nothing to total *)
              Game.Position.equal key a_position))
      then acc + 0
      else
        acc
        + List.fold
            (get_neighbors a_position)
            ~init:0
            ~f:(fun acc_for_point neighbor ->
              if not
                   (Map.existsi game.board ~f:(fun ~key ~data ->
                      ignore data;
                      Game.Position.equal key neighbor))
              then acc_for_point + 0
              else (
                match
                  Game.Piece.equal
                    piece_to_eval
                    (Map.find_exn game.board neighbor)
                with
                | true -> acc_for_point + 1
                | false -> acc_for_point - 1)))
  ;;

  let score game ~me ~depth maximizing_player ~evaluated_game =
    (* determine the heuristic value for a game currently in progress *)
    match evaluated_game with
    | Game.Evaluation.Game_over { winner } ->
      (match winner with
       | Some winner ->
         if Game.Piece.equal me winner
         then Int.max_value - 5 + depth
         else Int.min_value + 5 - depth
         | None -> 0)
    | Game.Evaluation.Game_continues ->
      let winning_positions = winning_moves ~me game in
      let losing_positions = losing_moves ~me game in
      let number_of_winning_moves = List.length winning_positions in
      let number_of_losing_moves = List.length losing_positions in
      (match maximizing_player with
       | true ->
         (match 0 = number_of_winning_moves, 0 = number_of_losing_moves with
          | true, true ->
            Int.max_value
            - 10
            - 5
            + depth
            + evaluate_how_many_consecutive_pieces_on_current_board
                game
                ~piece_to_eval:me
            (* funct is a higher int value if the piece passed in has more
               consecutive pieces of it than the other piece *)
          | true, false ->
            Int.min_value + 10 - depth - (2 * number_of_losing_moves)
          | false, true ->
            Int.max_value - 20 + depth + (2 * number_of_winning_moves)
          | _, _ ->
            Int.max_value
            - 15
            + depth
            -number_of_losing_moves + number_of_winning_moves)
       | false ->
         (match 0 = number_of_winning_moves, 0 = number_of_losing_moves with
          | true, true ->
            Int.min_value
            + 10
            + 5
            - depth
            - evaluate_how_many_consecutive_pieces_on_current_board
                game
                ~piece_to_eval:(Game.Piece.flip me)
          | true, false ->
            Int.min_value + 10 - depth - (2 * number_of_losing_moves)
          | false, true ->
            Int.max_value - 20 + depth + (2 * number_of_winning_moves)
          | _ ->
            Int.min_value
            + 15
            - depth
            - number_of_losing_moves
            + number_of_winning_moves))
    | _ -> 0
  ;;
  let _temp_gomoku_score game ~me ~depth maximizing_player ~evaluated_game =
    (* determine the heuristic value for a game currently in progress *)
    match evaluated_game with
    | Game.Evaluation.Game_over { winner } ->
      (match winner with
       | Some winner ->
         if Game.Piece.equal me winner
         then Int.max_value - 5 + depth
         else Int.min_value + 5 - depth
         | None -> 0)
    | Game.Evaluation.Game_continues ->
      (* let winning_positions = winning_moves ~me game in
      let losing_positions = losing_moves ~me game in
      let number_of_winning_moves = List.length winning_positions in
      let number_of_losing_moves = List.length losing_positions in *)
      (match maximizing_player with
       | true ->
         (* (match 0 = number_of_winning_moves, 0 = number_of_losing_moves with
          | true, true -> *)
            Int.max_value
            - 10
            - 5
            + depth
            + evaluate_how_many_consecutive_pieces_on_current_board
                game
                ~piece_to_eval:me
            (* funct is a higher int value if the piece passed in has more
               consecutive pieces of it than the other piece *)
          (* | true, false ->
            Int.min_value + 10 - depth - (2 * number_of_losing_moves)
          | false, true ->
            Int.max_value - 20 + depth + (2 * number_of_winning_moves)
          | _, _ ->
            Int.max_value
            - 15
            + depth
            -number_of_losing_moves + number_of_winning_moves) *)
       | false ->
         (* (match 0 = number_of_winning_moves, 0 = number_of_losing_moves with
          | true, true -> *)
            Int.min_value
            + 10
            + 5
            - depth
            - evaluate_how_many_consecutive_pieces_on_current_board
                game
                ~piece_to_eval:(Game.Piece.flip me))
          (* | true, false ->
            Int.min_value + 10 - depth - (2 * number_of_losing_moves)
          | false, true ->
            Int.max_value - 20 + depth + (2 * number_of_winning_moves)
          | _ ->
            Int.min_value
            + 15
            - depth
            - number_of_losing_moves
            + number_of_winning_moves)) *)
    | _ -> 0
  ;;
    (* let _gomoku_score game ~me ~depth maximizing_player ~evaluated_game =
    match evaluated_game with
    | Game.Evaluation.Game_over { winner } ->
      (match winner with
       | None -> 0
       | Some winner ->
         if Game.Piece.equal me winner
         then Int.max_value - 5 + depth
         else Int.min_value + 5 - depth)
    | Game.Evaluation.Game_continues ->
      let winning_positions = winning_moves ~me game in
      let losing_positions = losing_moves ~me game in
      let number_of_winning_moves = List.length winning_positions in
      let number_of_losing_moves = List.length losing_positions in
      if maximizing_player
      then (
        match number_of_winning_moves with
        | 0 ->
          (match number_of_losing_moves with
           | 0 ->
             Int.max_value
             - 15
             + depth
             + evaluate_how_many_consecutive_pieces_on_current_board
                 game
                 ~piece_to_eval:me
           | _ -> Int.min_value + 20 - depth - (2 * number_of_losing_moves))
        | _ -> Int.max_value - 10 + depth + (2 * number_of_winning_moves))
      else (match number_of_losing_moves with
      | 0 -> ()
      | _ -> )
    | _ -> 0 *)
  ;;

  let rec minimax game ?(depth = 2) ~me maximizing_player =

    let evaluated_game = evaluate game in
    let current_node_heuristic = 
      score game ~me ~depth maximizing_player ~evaluated_game
    in
    (* Core.print_endline "current heuristic: ";
    Core.print_s (Int.sexp_of_t current_node_heuristic); *)
    match depth = 0, evaluated_game with
    | true, _ | _, Game.Evaluation.Game_over { winner = _ } ->
      current_node_heuristic
    | _, _ ->
      if maximizing_player
      then (
        let next_possible_game_states_list =
          get_next_game_states game ~piece:me
        in
        List.fold
          next_possible_game_states_list
          ~init:Int.min_value
          ~f:(fun acc game_state ->
            let child_minimax =
              minimax
                game_state
                ~depth:(depth - 1)
                ~me
                (not maximizing_player)
            in
            if acc > child_minimax then acc else child_minimax))
      else (
        let next_possible_game_states_list =
          get_next_game_states game ~piece:(Game.Piece.flip me)
        in
        List.fold
          next_possible_game_states_list
          ~init:Int.max_value
          ~f:(fun acc game_state ->
            let child_minimax =
              minimax
                game_state
                ~depth:(depth - 1)
                ~me
                (not maximizing_player)
            in
            if acc < child_minimax then acc else child_minimax))
  ;;

  let use_minimax_to_find_best_move game ~me =
    (* Core.print_s (Game.sexp_of_t game); *)
    let possible_moves = available_moves game in

    let best_move, _heuristic =
      List.fold
        possible_moves
        ~init:({ Game.Position.row = 0; column = 0 }, Int.min_value)
        ~f:(fun (current_best_pos, current_highest_heuristic) move ->
          let heuristic_calculated =
            minimax (place_piece game ~piece:me ~position:move) ~me false
          in
          if heuristic_calculated > current_highest_heuristic
          then move, heuristic_calculated
          else current_best_pos, current_highest_heuristic)
    in
    best_move
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_five =
    Command.async
      ~summary:
        "Exercise 5: Is there available move that do not immediately lose?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let available_moves_that_do_not_immediately_lose =
           available_moves_that_do_not_immediately_lose ~me:piece non_win
         in
         print_s
           [%sexp
             (available_moves_that_do_not_immediately_lose
              : Game.Position.t list)];
         return ())
  ;;

  let exercise_six =
    Command.async
      ~summary:
        "Exercise 6: What is the best next move for the piece passed in?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let minimaxed = use_minimax_to_find_best_move ~me:piece non_win in
         print_s [%sexp (minimaxed : Game.Position.t)];
         return ())
  ;;

  let make_move game piece position = place_piece game ~piece ~position

  let exercise_seven =
    Command.async
      ~summary:"Exercise 7: Omok against self"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         Core.print_s [%message "starting"];
         let list = List.init 225 ~f:(fun i -> i) in
         Core.print_s [%message (List.length list : int)];
         let _winner =
           List.fold
             list
             ~init:(Game.empty Game.Game_kind.Omok, piece)
             ~f:(fun (board, piece) _num ->
               Core.print_s [%message "BOARD: "];
               print_game board;
               let best_move =
                 use_minimax_to_find_best_move board ~me:piece 
               in
               Core.print_s [%message (best_move : Game.Position.t)];
               let new_board, next_piece =
                 make_move board piece best_move, Game.Piece.flip piece
               in
               print_game new_board;
               new_board, next_piece)
         in
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ; "five", exercise_five
      ; "six", exercise_six
      ; "seven", exercise_seven
      ]
  ;;
end

let handle_turn (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  print_s [%message "Received query" (query : Rpcs.Take_turn.Query.t)];
  let response =
    { Rpcs.Take_turn.Response.piece = query.you_play
    ; Rpcs.Take_turn.Response.position =
        Exercises.use_minimax_to_find_best_move query.game ~me:query.you_play
    }
  in
  return response
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle
          incoming queries for [Take_turn] and [Game_over]. We should also
          connect to the controller and send a [Start_game] to initiate the
          game. *)
       let%bind server =
         let implementations =
           Rpc.Implementations.create_exn
             ~on_unknown_rpc:`Close_connection
             ~implementations:
               [ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle_turn ]
         in
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;

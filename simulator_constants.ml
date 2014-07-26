module Delay = struct
  let eating = 127
  let not_eating = 137

  let ghost = [| 	130; 132; 134; 136 |]
  let ghost_fright = [| 195; 198; 201; 204 |]
end

module Time = struct
  let fruit_1_appear = 127 * 200
  let fruit_2_appear = 127 * 400
  let fruit_1_expires = 127 * 280
  let fruit_2_expires = 127 * 480
  let fright_mode_duration = 127 * 20
end

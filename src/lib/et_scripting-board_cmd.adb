------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          SCRIPTING IN BOARD                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

separate (et_scripting)
	
function board_cmd (
	cmd				: in type_fields_of_line; -- "board tree_1 draw silk top line 2.5 0 0 160 0"
	log_threshold	: in type_log_level)
	return type_exit_code is

	use et_board_ops;
	use et_packages;
	use et_terminals.pac_shapes;
	use et_pcb;
	use et_pcb_coordinates;
	use et_pcb_coordinates.pac_geometry_brd;
	use et_pcb_stack;
	use et_canvas_board.pac_canvas;
	use et_display.board;

	-- The exit code will be overridden with ERROR or WARNING if something goes wrong:
	exit_code : type_exit_code := SUCCESSFUL;
	
	domain	: type_domain; -- DOM_BOARD
	module	: type_module_name.bounded_string; -- motor_driver (without extension *.mod)

	verb	: type_verb_board;
	noun	: type_noun_board;
	
	
	function f (place : in positive) return string is begin
		return et_string_processing.field (cmd, place);
	end;

	function fields return count_type is begin
		return et_string_processing.field_count (cmd);
	end;

	procedure too_long is begin -- CS use it more often
		command_too_long (cmd, fields - 1);
	end;

	-- Enables/disables the grid "layer". If status is empty,
	-- the layer will be enabled.
	procedure display_grid ( -- GUI related
		status	: in string := "") is 

		ls : type_layer_status;
	begin
		-- Convert the given status to type_layer_status.
		-- If no status given, assume status ON:
		if status = "" then
			ls := ON;
		else
			ls := to_layer_status (status);
		end if;

		log (text => "display grid layer" & space & to_string (ls),
				level => log_threshold + 1);

		layers.grid := ls;
		
		-- CS exception handler if status is invalid
	end display_grid;
	
	-- Enables/disables the outline layer. If status is empty,
	-- the layer will be enabled.
	procedure display_outline ( -- GUI related
		status	: in string := "") is 

		ls : type_layer_status;
	begin
		-- Convert the given status to type_layer_status.
		-- If no status given, assume status ON:
		if status = "" then
			ls := ON;
		else
			ls := to_layer_status (status);
		end if;

		log (text => "display outline layer" & space & to_string (ls),
				level => log_threshold + 1);

		layers.outline := ls;
		
		-- CS exception handler if status is invalid
	end display_outline;
	
	-- Enables/disables a certain non-conductor layer. If status is empty,
	-- the layer will be enabled.
	procedure display_non_conductor_layer ( -- GUI related
		layer	: in type_noun_board;
		face	: in string; -- top/bottom
		status	: in string := "") is 

		ls : type_layer_status;
		fc : type_face;
	begin
		-- Convert the given status to type_layer_status.
		-- If no status given, assume status ON:
		if status = "" then
			ls := ON;
		else
			ls := to_layer_status (status);
		end if;

		-- Convert the given face to type_face:
		fc := to_face (face);
		
		log (text => "display " & to_lower (to_string (layer)) 
				& space & to_string (ls),
				level => log_threshold + 1);

		case fc is
			when TOP =>
				case layer is
					when NOUN_SILKSCREEN 	=> layers.silkscreen.top		:= ls;
					when NOUN_ASSY			=> layers.assy_doc.top			:= ls;
					when NOUN_KEEPOUT		=> layers.keepout.top			:= ls;
					when NOUN_STENCIL		=> layers.stencil.top			:= ls;
					when NOUN_STOP			=> layers.stop_mask.top			:= ls;
					when NOUN_ORIGINS		=> layers.device_origins.top	:= ls;
					
					when others => 
						log (importance => ERROR, text => "invalid layer !", console => true);
				end case;

			when BOTTOM =>
				case layer is
					when NOUN_SILKSCREEN 	=> layers.silkscreen.bottom		:= ls;
					when NOUN_ASSY			=> layers.assy_doc.bottom		:= ls;
					when NOUN_KEEPOUT		=> layers.keepout.bottom		:= ls;
					when NOUN_STENCIL		=> layers.stencil.bottom		:= ls;
					when NOUN_STOP			=> layers.stop_mask.bottom		:= ls;
					when NOUN_ORIGINS		=> layers.device_origins.bottom	:= ls;
					
					when others => 
						log (importance => ERROR, text => "invalid layer !", console => true);
				end case;
		end case;
		
		-- CS exception handler if status is invalid
	end display_non_conductor_layer;

	-- Enables/disables a certain conductor layer. 
	-- If status is empty, the layer will be enabled.
	procedure display_conductor_layer ( -- GUI related
		layer	: in string;
		status	: in string := "") is 

		ls : type_layer_status;
		ly : type_signal_layer;
	begin
		-- Convert the given status to type_layer_status.
		-- If no status given, assume status ON:
		if status = "" then
			ls := ON;
		else
			ls := to_layer_status (status);
		end if;

		-- Convert the given layer to type_signal_layer:
		ly := to_signal_layer (layer);
		
		log (text => "display conductor layer " & to_string (ly) & space & to_string (ls),
				level => log_threshold + 1);

		layers.conductors (ly) := ls;
		
		-- CS exception handler if status is invalid
	end display_conductor_layer;

	-- Enables/disables a the via layer. 
	-- If status is empty, the layer will be enabled.
	procedure display_vias ( -- GUI related
		--layer	: in string;
		status	: in string := "") is 

		ls : type_layer_status;
		--ly : type_signal_layer;
	begin
		-- Convert the given status to type_layer_status.
		-- If no status given, assume status ON:
		if status = "" then
			ls := ON;
		else
			ls := to_layer_status (status);
		end if;

		-- Convert the given layer to type_signal_layer:
		--ly := to_signal_layer (layer);
		
		--log (text => "display via layer " & to_string (ly) & space & to_string (ls),
		log (text => "display via layer " & space & to_string (ls),
				level => log_threshold + 1);

		--layers.vias (ly) := ls;
		layers.vias := ls;
		
		-- CS exception handler if status is invalid
	end display_vias;
	
	-- Enables/disables a certain restrict layer. 
	-- If status is empty, the layer will be enabled.
	procedure display_restrict_layer ( -- GUI related
		objects	: in string; -- route/via
		layer	: in string; -- 1, 2, 8, ...
		status	: in string := "") is 

		ls : type_layer_status;
		ly : type_signal_layer;
	begin
		-- Convert the given status to type_layer_status.
		-- If no status given, assume status ON:
		if status = "" then
			ls := ON;
		else
			ls := to_layer_status (status);
		end if;

		-- Convert the given layer to type_signal_layer:
		ly := to_signal_layer (layer);
		
		if objects = keyword_route then
			log (text => "display route restrict layer " & to_string (ly) & space & to_string (ls),
				level => log_threshold + 1);

			layers.route_restrict (ly) := ls;
			
		elsif objects = keyword_via then
			log (text => "display via restrict layer " & to_string (ly) & space & to_string (ls),
				level => log_threshold + 1);

			layers.via_restrict (ly) := ls;
			
		else
			log (importance => ERROR, 
					text => "Expect keyword " &
					enclose_in_quotes (keyword_route) & " or " &
					enclose_in_quotes (keyword_via) & "after noun " &
					to_string (NOUN_RESTRICT) & " !",
					console => true);
			raise constraint_error;
		end if;
		
		-- CS exception handler if status is invalid
	end display_restrict_layer;
	

	procedure draw_keepout is
		shape : type_shape := to_shape (f (6));
	begin
		case shape is
			when LINE =>
				case fields is
					when 10 =>
						draw_keepout_line (
							module_name 	=> module,
							face			=> to_face (f (5)),
							line			=> (
										start_point	=> type_point (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
										end_point	=> type_point (set (
											x => to_distance (f (9)),
											y => to_distance (f (10))))
										),

							log_threshold	=> log_threshold + 1);

					when 11 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;
				
			when ARC =>
				case fields is
					when 13 =>
						draw_keepout_arc (
							module_name 	=> module,
							face			=> to_face (f (5)),
							arc				=> (
										center	=> type_point (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
										start_point	=> type_point (set (
											x => to_distance (f (9)),
											y => to_distance (f (10)))),
										end_point	=> type_point (set (
											x => to_distance (f (11)),
											y => to_distance (f (12)))),
										direction	=> to_direction (f (13))
										),

							log_threshold	=> log_threshold + 1);

					when 14 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;

			when CIRCLE =>
				case fields is
					when 9 =>
					-- board led_driver draw keepout top circle 50 50 40 -- 9 fields
						
						if is_number (f (7)) then
							-- Circle is not filled.
							
							draw_keepout_circle (
								module_name 	=> module,
								face			=> to_face (f (5)),
								circle			=> 
											(
											filled		=> NO,
											center	=> type_point (set (
														x => to_distance (f (7)),
														y => to_distance (f (8)))),
											radius	=> to_distance (f (9))
											),
								log_threshold	=> log_threshold + 1);
						else
							expect_value_center_x (7);
						end if;

					when 10 =>
					-- board led_driver draw keepout top circle filled 50 50 40 -- 10 fields
						
						if f (7) = keyword_filled then
							-- Circle is filled.
							
							draw_keepout_circle (
								module_name 	=> module,
								face			=> to_face (f (5)),
								circle			=> 
											(
											filled		=> YES,
											center	=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
											radius	=> to_distance (f (10))
											),
								log_threshold	=> log_threshold + 1);
						else
							expect_keyword_filled (7);
						end if;
							
					when 12 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;

						
			when others => null;
		end case;
	end draw_keepout;
	
	procedure draw_route_restrict is
		shape : type_shape := to_shape (f (6));
	begin
		case shape is
			when LINE =>
				case fields is
					when 10 =>
						-- board led_driver draw route_restrict [1,3,5-9] line 10 10 60 10
						-- CS board led_driver draw route_restrict 3 line 10 10 60 10
						
						-- CS test whether field 5 is a single layer id. If yes then
						-- call function et_pcb_stack.to_signal_layer to get the id-type.
						-- Then validate signal layer.
						-- Then add the single signal layer to a set.
						-- Do so with all objects in route and via restrict.
						
						draw_route_restrict_line (
							module_name 	=> module,
							line			=> (
										layers		=> to_layers (f (5)), -- [1,3,5-9]
										start_point	=> type_point (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
										end_point	=> type_point (set (
											x => to_distance (f (9)),
											y => to_distance (f (10))))
										),

							log_threshold	=> log_threshold + 1);

					when 11 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;
				
			when ARC =>
				case fields is
					when 13 =>
						-- board led_driver draw route_restrict [1,3,5-9] arc 50 50 0 50 100 0 cw
						draw_route_restrict_arc (
							module_name 	=> module,
							arc				=> (
										layers		=> to_layers (f (5)), -- [1,3,5-9]
										center	=> type_point (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
										start_point	=> type_point (set (
											x => to_distance (f (9)),
											y => to_distance (f (10)))),
										end_point	=> type_point (set (
											x => to_distance (f (11)),
											y => to_distance (f (12)))),
										direction	=> to_direction (f (13))
										),

							log_threshold	=> log_threshold + 1);

					when 14 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;

			when CIRCLE =>
				case fields is
					when 9 =>
						-- board led_driver draw route_restrict [1,3,5-9] circle 20 50 40
						if is_number (f (7)) then -- 20

							-- Circle is not filled.
							draw_route_restrict_circle (
								module_name 	=> module,
								circle			=> 
											(
											layers		=> to_layers (f (5)), -- [1,3,5-9]
											filled		=> NO,
											center	=> type_point (set (
														x => to_distance (f (7)), -- 20
														y => to_distance (f (8)))), -- 50
											radius	=> to_distance (f (9)) -- 40
											),
											
								log_threshold	=> log_threshold + 1);
						else
							expect_value_center_x (7);
						end if;

					when 10 =>
						-- Circle is filled.
						-- board led_driver draw route_restrict [1,3,5-9] circle filled 20 50 40
						if f (7) = keyword_filled then

							-- Circle is filled.
							draw_route_restrict_circle (
								module_name 	=> module,
								circle			=> 
											(
											layers		=> to_layers (f (5)), -- [1,3,5-9]
											filled		=> YES,
											center	=> type_point (set (
														x => to_distance (f (8)), -- 20
														y => to_distance (f (9)))), -- 50
											radius	=> to_distance (f (10)) -- 40
											),
											
								log_threshold	=> log_threshold + 1);
						else
							expect_keyword_filled (7);
						end if;

					when 11 .. count_type'last => command_too_long (cmd, fields - 1);
					
					when others => command_incomplete (cmd);
				end case;
						
			when others => null;
		end case;
	end draw_route_restrict;

	procedure draw_via_restrict is
		shape : type_shape := to_shape (f (6));
	begin
		case shape is
			when LINE =>
				case fields is
					when 10 =>
						-- board led_driver draw via_restrict [1,3,5-9] line 10 10 60 10
						draw_via_restrict_line (
							module_name 	=> module,
							line			=> (
										layers		=> to_layers (f (5)), -- [1,3,5-9]
										start_point	=> type_point (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
										end_point	=> type_point (set (
											x => to_distance (f (9)),
											y => to_distance (f (10))))
										),

							log_threshold	=> log_threshold + 1);

					when 11 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;
				
			when ARC =>
				case fields is
					when 13 =>
						-- board led_driver draw via_restrict [1,3,5-9] arc 50 50 0 50 100 0
						draw_via_restrict_arc (
							module_name 	=> module,
							arc				=> (
										layers		=> to_layers (f (5)), -- [1,3,5-9]
										center	=> type_point (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
										start_point	=> type_point (set (
											x => to_distance (f (9)),
											y => to_distance (f (10)))),
										end_point	=> type_point (set (
											x => to_distance (f (11)),
											y => to_distance (f (12)))),
										direction	=> to_direction (f (13))
										),

							log_threshold	=> log_threshold + 1);

					when 14 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;

			when CIRCLE =>
				case fields is
					when 9 =>
						-- board led_driver draw via_restrict [1,3,5-9] circle 20 50 40
						if is_number (f (7)) then -- 20

							-- Circle is not filled.
							draw_via_restrict_circle (
								module_name 	=> module,
								circle			=> 
											(
											layers		=> to_layers (f (5)), -- [1,3,5-9]
											filled		=> NO,
											center	=> type_point (set (
														x => to_distance (f (7)), -- 20
														y => to_distance (f (8)))), -- 50
											radius	=> to_distance (f (9)) -- 40
											),
											
								log_threshold	=> log_threshold + 1);
						else
							expect_value_center_x (7);
						end if;

					when 10 =>
						-- Circle is filled.
						-- board led_driver draw via_restrict [1,3,5-9] circle filled 20 50 40
						if f (7) = keyword_filled then

							-- Circle is filled.
							draw_via_restrict_circle (
								module_name 	=> module,
								circle			=> 
											(
											layers		=> to_layers (f (5)), -- [1,3,5-9]
											filled		=> YES,
											center	=> type_point (set (
														x => to_distance (f (8)), -- 20
														y => to_distance (f (9)))), -- 50
											radius	=> to_distance (f (10)) -- 40
											),
											
								log_threshold	=> log_threshold + 1);
						else
							expect_keyword_filled (7);
						end if;

					when 11 .. count_type'last => command_too_long (cmd, fields - 1);
					
					when others => command_incomplete (cmd);
				end case;
						
			when others => null;
		end case;
	end draw_via_restrict;

	procedure draw_stop_mask is
		use et_terminals.pac_shapes;
		shape : type_shape := to_shape (f (6));
	begin
		case shape is
			when LINE =>
				case fields is
					when 11 =>
						draw_stop_line (
							module_name 	=> module,
							face			=> to_face (f (5)),
							line			=> (
										width		=> to_distance (f (7)),
										start_point	=> type_point (set (
											x => to_distance (f (8)),
											y => to_distance (f (9)))),
										end_point	=> type_point (set (
											x => to_distance (f (10)),
											y => to_distance (f (11))))
										),

							log_threshold	=> log_threshold + 1);

					when 12 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;
				
			when ARC =>
				case fields is
					when 14 =>
						draw_stop_arc (
							module_name 	=> module,
							face			=> to_face (f (5)),
							arc				=> (
										width	=> to_distance (f (7)),
										center	=> type_point (set (
											x => to_distance (f (8)),
											y => to_distance (f (9)))),
										start_point	=> type_point (set (
											x => to_distance (f (10)),
											y => to_distance (f (11)))),
										end_point	=> type_point (set (
											x => to_distance (f (12)),
											y => to_distance (f (13)))),
										direction	=> to_direction (f (14))
										),

							log_threshold	=> log_threshold + 1);

					when 15 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;

			when CIRCLE =>
				case fields is
					when 10 =>

					-- The 7th field can either be a line width like 2.5 or a 
					-- fill style like CUTOUT or SOLID. HATCHED is not allowed here:
						if is_number (f (7)) then

							-- Circle is not filled and has a circumfence line width
							-- specified in field 7.
							draw_stop_circle (
								module_name 	=> module,
								face			=> to_face (f (5)),
								circle			=> 
										(
										filled			=> NO,
										fill_style		=> fill_style_default, -- don't care here
										border_width	=> to_distance (f (7)),
										center			=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
										radius			=> to_distance (f (10))
										),
								log_threshold	=> log_threshold + 1);
						else
							
							-- Circle is filled with the fill style specified in field 7:
							case to_fill_style (f (7)) is
-- CS
-- 										when CUTOUT =>
-- 									
-- 											draw_stop_circle (
-- 												module_name 	=> module,
-- 												face			=> to_face (f (5)),
-- 												circle			=> 
-- 															(
-- 															filled		=> YES,
-- 															fill_style	=> CUTOUT,
-- 															center	=> type_point (set (
-- 																		x => to_distance (f (8)),
-- 																		y => to_distance (f (9)))),
-- 															radius	=> to_distance (f (10))
-- 															),
-- 												log_threshold	=> log_threshold + 1);

								when SOLID =>
							
									draw_stop_circle (
										module_name 	=> module,
										face			=> to_face (f (5)),
										circle			=> 
													(
													filled		=> YES,
													fill_style	=> SOLID,
													center	=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
													radius	=> to_distance (f (10))
													),
										log_threshold	=> log_threshold + 1);

								when HATCHED => command_incomplete (cmd);

							end case;
						end if;
							
					when 12 =>
						-- This is going to be a hatched circle.
						-- In this case the 7th field MUST be fill style HATCHED.
						if is_number (f (7)) then
							expect_fill_style (HATCHED, 7); -- error
						else
							case to_fill_style (f (7)) is
								when HATCHED =>
									draw_stop_circle (
										module_name 	=> module,
										face			=> to_face (f (5)),
										circle			=> 
												(
												filled		=> YES,
												fill_style	=> HATCHED,
												center		=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
												radius		=> to_distance (f (10)),
												hatching	=> (
															line_width	=> to_distance (f (11)),
															spacing		=> to_distance (f (12)),
															others	=> <>
															)
												),
										log_threshold	=> log_threshold + 1);

								when others => expect_fill_style (HATCHED, 7);
							end case;
						end if;

					when 13 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;

						
			when others => null;
		end case;
	end draw_stop_mask;

	procedure draw_stencil is
		use et_terminals.pac_shapes;
		shape : type_shape := to_shape (f (6));
	begin
		case shape is
			when LINE =>
				case fields is
					when 11 =>
						draw_stencil_line (
							module_name 	=> module,
							face			=> to_face (f (5)),
							line			=> (
										width		=> to_distance (f (7)),
										start_point	=> type_point (set (
											x => to_distance (f (8)),
											y => to_distance (f (9)))),
										end_point	=> type_point (set (
											x => to_distance (f (10)),
											y => to_distance (f (11))))
										),

							log_threshold	=> log_threshold + 1);

					when 12 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;
				
			when ARC =>
				case fields is
					when 14 =>
						draw_stencil_arc (
							module_name 	=> module,
							face			=> to_face (f (5)),
							arc				=> (
										width	=> to_distance (f (7)),
										center	=> type_point (set (
											x => to_distance (f (8)),
											y => to_distance (f (9)))),
										start_point	=> type_point (set (
											x => to_distance (f (10)),
											y => to_distance (f (11)))),
										end_point	=> type_point (set (
											x => to_distance (f (12)),
											y => to_distance (f (13)))),
										direction	=> to_direction (f (14))
										),

							log_threshold	=> log_threshold + 1);

					when 15 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;

			when CIRCLE =>
				case fields is
					when 10 =>

					-- The 7th field can either be a line width like 2.5 or a 
					-- fill style like CUTOUT or SOLID. HATCHED is not allowed here:
						if is_number (f (7)) then

							-- Circle is not filled and has a circumfence line width
							-- specified in field 7.
							draw_stencil_circle (
								module_name 	=> module,
								face			=> to_face (f (5)),
								circle			=> 
										(
										filled			=> NO,
										fill_style		=> fill_style_default, -- don't care here
										border_width	=> to_distance (f (7)),
										center			=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
										radius			=> to_distance (f (10))
										),
								log_threshold	=> log_threshold + 1);
						else
							
							-- Circle is filled with the fill style specified in field 7:
							case to_fill_style (f (7)) is
-- CS
-- 										when CUTOUT =>
-- 									
-- 											draw_stencil_circle (
-- 												module_name 	=> module,
-- 												face			=> to_face (f (5)),
-- 												circle			=> 
-- 															(
-- 															filled		=> YES,
-- 															fill_style	=> CUTOUT,
-- 															center	=> type_point (set (
-- 																		x => to_distance (f (8)),
-- 																		y => to_distance (f (9)))),
-- 															radius	=> to_distance (f (10))
-- 															),
-- 												log_threshold	=> log_threshold + 1);

								when SOLID =>
							
									draw_stencil_circle (
										module_name 	=> module,
										face			=> to_face (f (5)),
										circle			=> 
													(
													filled		=> YES,
													fill_style	=> SOLID,
													center	=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
													radius	=> to_distance (f (10))
													),
										log_threshold	=> log_threshold + 1);

								when HATCHED => command_incomplete (cmd);

							end case;
						end if;
							
					when 12 =>
						-- This is going to be a hatched circle.
						-- In this case the 7th field MUST be fill style HATCHED.
						if is_number (f (7)) then
							expect_fill_style (HATCHED, 7); -- error
						else
							case to_fill_style (f (7)) is
								when HATCHED =>
									draw_stencil_circle (
										module_name 	=> module,
										face			=> to_face (f (5)),
										circle			=> 
												(
												filled		=> YES,
												fill_style	=> HATCHED,
												center		=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
												radius		=> to_distance (f (10)),
												hatching	=> (
															line_width	=> to_distance (f (11)),
															spacing		=> to_distance (f (12)),
															others		=> <>
															)
												),
										log_threshold	=> log_threshold + 1);

								when others => expect_fill_style (HATCHED, 7);
							end case;
						end if;

					when 13 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others => command_incomplete (cmd);
				end case;

						
			when others => null;
		end case;
	end draw_stencil;
	
	-- CS circular tracks are currently not supported
	subtype type_track_shape is type_shape range LINE..ARC;
	
	procedure route_net is 
		use et_terminals;
		shape : type_track_shape := to_shape (f (7));
	begin
		case shape is
			when LINE =>
				if is_number (f (9)) then -- 33.4 or IC4
					
					-- THE TRACK STARTS AT A DEDICATED POINT AT X/Y:
					
					-- board motor_driver route net NET_1 2 line 0.25 0 0 160 0
					case fields is
						when 12 =>
							draw_track_line (
								module_name 	=> module,
								net_name		=> to_net_name (f (5)),
								line	=> (
									layer		=> to_signal_layer (f (6)),
									width		=> to_distance (f (8)),
									start_point	=> type_point (set (
										x => to_distance (f (9)),
										y => to_distance (f (10)))),
									end_point	=> type_point (set (
										x => to_distance (f (11)),
										y => to_distance (f (12))))
									),
								
								log_threshold	=> log_threshold + 1
								);

						when 13 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				else
					-- THE TRACK STARTS AT A TERMINAL:
					
					if f (11) = keyword_to then
						-- board motor_driver route net NET_1 1 line 0.25 R1 1 to 35 40
						-- board motor_driver route net NET_1 1 line 0.25 R1 1 to x 5
						
						if is_number (f (12)) then
							-- THE TRACK ENDS AT A DEDICATED POINT X/Y
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 to 35 40
							case fields is
								when 13 =>
									draw_track_line (
										module_name => module,
										net_name	=> to_net_name (f (5)),
										layer		=> to_signal_layer (f (6)),
										width		=> to_distance (f (8)),
										device		=> to_name (f (9)),
										terminal	=> to_terminal_name (f (10)),
										end_point	=> type_point (set (
												x => to_distance (f (12)),	 -- 35
												y => to_distance (f (13)))), -- 40
										
										log_threshold	=> log_threshold + 1
										);
									
								when 14 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others =>
									command_incomplete (cmd);
							end case;
									
						else
							-- THE TRACK ENDS ON A GRID LINE ALONG A GIVEN AXIS:
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 to x 5
							if f (12) = to_string (X) or f (12) = to_string (Y) then
								case fields is
									when 13 =>
										draw_track_line (
											module_name => module,
											net_name	=> to_net_name (f (5)),
											layer		=> to_signal_layer (f (6)),
											width		=> to_distance (f (8)),
											device		=> to_name (f (9)),
											terminal	=> to_terminal_name (f (10)),
											axis		=> to_axis (f (12)),
											notches		=> to_notches (f (13)), -- 5
											
											log_threshold	=> log_threshold + 1
											);
										
									when 14 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;
								
							else
								invalid_keyword (12);
							end if;
						end if;
						
						
					elsif f (11) = keyword_direction then
						-- THE TRACK RUNS INTO GIVEN DIRECTION SPECIFIED BY AN ANGLE
						
						if is_number (f (13)) then
							-- THE TRACK ENDS AFTER A GIVEN DISTANCE (it has a given length)
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 direction 45 50
							
							case fields is
								when 13 =>
									draw_track_line (
										module_name => module,
										net_name	=> to_net_name (f (5)),
										layer		=> to_signal_layer (f (6)),
										width		=> to_distance (f (8)),
										device		=> to_name (f (9)),
										terminal	=> to_terminal_name (f (10)),
										direction	=> to_rotation (f (12)), -- 45 degree
										length		=> to_distance (f (13)), -- 50mm
										
										log_threshold	=> log_threshold + 1
										);

								when 14 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others =>
									command_incomplete (cmd);
							end case;

						else
							-- THE TRACK ENDS AT A GIVEN GRID LINE ALONG A GIVEN AXIS
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 direction 45 x 5
							if f (13) = to_string (X) or f (13) = to_string (Y) then
								
								case fields is
									when 14 =>
										draw_track_line (
											module_name => module,
											net_name	=> to_net_name (f (5)),
											layer		=> to_signal_layer (f (6)),
											width		=> to_distance (f (8)),
											device		=> to_name (f (9)),
											terminal	=> to_terminal_name (f (10)),
											direction	=> to_rotation (f (12)), -- 45 degree
											axis		=> to_axis (f (13)),
											notches		=> to_notches (f (14)), -- 5
											
											log_threshold	=> log_threshold + 1
											);

									when 15 .. count_type'last => command_too_long (cmd, fields - 1);
										
									when others => 
										command_incomplete (cmd);
								end case;
								
							else
								invalid_keyword (13);
							end if;
						end if;

					else
						invalid_keyword (11);
					end if;
				end if;
				
			when ARC =>
				case fields is
					when 15 =>
						-- draw a named track
						draw_track_arc (
							module_name 	=> module,
							net_name		=> to_net_name (f (5)),
							arc		=> (
								layer		=> to_signal_layer (f (6)),
								width		=> to_distance (f (8)),
								center		=> type_point (set (
									x => to_distance (f (9)),
									y => to_distance (f (10)))),
								start_point	=> type_point (set (
									x => to_distance (f (11)),
									y => to_distance (f (12)))),
								end_point	=> type_point (set (
									x => to_distance (f (13)),
									y => to_distance (f (14)))),
								direction	=> to_direction (f (15))
								),

							log_threshold	=> log_threshold + 1
							);
						
					when 16 .. count_type'last => command_too_long (cmd, fields - 1);
						
					when others =>
						command_incomplete (cmd);
				end case;

		end case;
	end route_net;

	procedure add_layer is
		use et_pcb_stack;
		layer : type_layer;
	begin
		-- board tree_1 add layer 0.12 0.2				
		layer.conductor.thickness := to_distance (f (5));
		layer.dielectric.thickness := to_distance (f (6));
		
		add_layer (
			module_name 	=> module,
			layer			=> layer,
			log_threshold	=> log_threshold + 1);

	end add_layer;

	procedure zoom_center is -- GUI related
		-- Build the center point:
		c : type_point := type_point (set (
				x => to_distance (f (5)),
				y => to_distance (f (6))));
	begin
		log (text => "center on point", level => log_threshold + 1);
		center_on (canvas, c);
	end zoom_center;

	procedure set_scale (scale : in string) is  -- GUI related -- CS should be percent of scale_to_fit
		use glib;
		s : gdouble := gdouble'value (scale);
	begin
		log (text => "zoom level", level => log_threshold + 1);
		set_scale (canvas, s);
	end set_scale;
	
	-- Positions the cursor absolute or relative:
	procedure position_cursor is -- GUI related
		use et_geometry;
		
		coordinates : type_coordinates := to_coordinates (f (5));
		position : type_point := type_point (set (
				x => to_distance (f (6)),
				y => to_distance (f (7))));
	begin
		log (text => "place cursor" & to_string (coordinates) 
			& to_string (position), level => log_threshold + 1);
		
		canvas.move_cursor (coordinates, cursor_main, position);
	end position_cursor;

	procedure add_device is -- non-electric device !
		-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5
		-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0
		-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0 top

		model : constant type_package_model_file.bounded_string := to_file_name (f (5));
		prefix : constant type_prefix.bounded_string := to_prefix (f (6));

		xy : constant type_point := type_point (set (
				x => to_distance (f (7)),
				y => to_distance (f (8))));

	begin
		case fields is
			when 8 =>
				add_device (
					module_name		=> module,
					package_model	=> model,
					position		=> to_package_position
						(
						point	=> xy
						),
					prefix			=> prefix,
					log_threshold	=> log_threshold + 1);

			when 9 =>
				add_device (
					module_name		=> module,
					package_model	=> model,
					position		=> to_package_position
						(
						point		=> xy,
						rotation	=> to_rotation (f (9))
						),
					prefix			=> prefix,
					log_threshold	=> log_threshold + 1);

			when 10 =>
				add_device (
					module_name		=> module,
					package_model	=> model,
					position		=> to_package_position
						(
						point		=> xy,
						rotation	=> to_rotation (f (9)),
						face		=> to_face (f (10))
						),
					prefix			=> prefix,
					log_threshold	=> log_threshold + 1);
				
			when others => raise constraint_error; -- CS should never happen
		end case;
	end add_device;

	procedure delete_device is -- non-electric device !
		-- board led_driver delete device FD1
	begin
		delete_device (
			module_name		=> module,
			device_name		=> to_name (f (5)),
			log_threshold	=> log_threshold + 1);

	end delete_device;

	procedure rename_device is -- non-electric device !
		-- board led_driver rename device FD1 FD3
	begin
		rename_device (
			module_name			=> module,
			device_name_before	=> to_name (f (5)),
			device_name_after	=> to_name (f (6)),
			log_threshold		=> log_threshold + 1);

	end rename_device;

	
begin -- board_cmd
	log (text => "full command: " & enclose_in_quotes (to_string (cmd)), level => log_threshold);

	domain := to_domain (f (1)); -- DOM_BOARD
	module := to_module_name (f (2)); -- motor_driver (without extension *.mod)

	-- read the verb from field 3
	verb := to_verb (f (3));

	-- There are some very short commands which do not require a verb.
	-- For such commands we do not read the noun.
	case verb is
		when VERB_EXIT | VERB_QUIT => null; -- no noun
		when others => noun := to_noun (f (4)); -- read noun from field 4
	end case;

	-- parse the command:
	case verb is
		when VERB_ADD =>
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 8..10 => add_device;
						-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5
						-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0
						-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0 top

						when 11 .. count_type'last => too_long;
						
						when others => command_incomplete (cmd);
					end case;

				when NOUN_LAYER =>
					case fields is
						when 6 =>
							-- board tree_1 add layer 0.12 0.2
							add_layer;

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_DELETE =>
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 5 => delete_device; -- board led_driver delete device FD1

						when 6 .. count_type'last => too_long;
						
						when others => command_incomplete (cmd);
					end case;

				when NOUN_LAYER =>
					case fields is
						when 5 =>
							-- board tree_1 delete layer 2
							delete_layer (
								module_name 	=> module,
								layer			=> to_signal_layer (f (5)),
								
								log_threshold	=> log_threshold + 1);

						when 6 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);

					end case;

				when NOUN_OUTLINE =>
					case fields is
						when 7 =>
							-- delete a segment of board outline
							delete_outline (
								module_name 	=> module,
								point			=> type_point (set (
										x => to_distance (f (5)),
										y => to_distance (f (6)))),
								accuracy		=> to_distance (f (7)),
								
								log_threshold	=> log_threshold + 1);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_SILKSCREEN =>
					-- board led_driver delete silkscreen top 40 50 1
					case fields is
						when 8 =>
							-- delete a segment of silk screen
							delete_silk_screen (
								module_name 	=> module,
								face			=> to_face (f (5)),
								point			=> type_point (set (
										x => to_distance (f (6)),
										y => to_distance (f (7)))),
								accuracy		=> to_distance (f (8)),
								
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last =>
							command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;

				when NOUN_ASSY =>
					-- board led_driver delete assy top 40 50 1
					case fields is
						when 8 =>
							-- delete a segment of assembly documentation
							delete_assy_doc (
								module_name 	=> module,
								face			=> to_face (f (5)),
								point			=> type_point (set (
										x => to_distance (f (6)),
										y => to_distance (f (7)))),
								accuracy		=> to_distance (f (8)),
								
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_KEEPOUT =>
					-- board led_driver delete keepout top 40 50 1
					case fields is
						when 8 =>
							-- delete a segment of keepout
							delete_keepout (
								module_name 	=> module,
								face			=> to_face (f (5)),
								point			=> type_point (set (
										x => to_distance (f (6)),
										y => to_distance (f (7)))),
								accuracy		=> to_distance (f (8)),
								
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_STENCIL =>
					-- board led_driver delete stencil top 40 50 1
					case fields is
						when 8 =>
							-- delete a segment of stencil
							delete_stencil (
								module_name 	=> module,
								face			=> to_face (f (5)),
								point			=> type_point (set (
										x => to_distance (f (6)),
										y => to_distance (f (7)))),
								accuracy		=> to_distance (f (8)),
								
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_STOP =>
					-- board led_driver delete stop top 40 50 1
					case fields is
						when 8 =>
							-- delete a segment of stop mask
							delete_stop (
								module_name 	=> module,
								face			=> to_face (f (5)),
								point			=> type_point (set (
										x => to_distance (f (6)),
										y => to_distance (f (7)))),
								accuracy		=> to_distance (f (8)),
								
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_ROUTE_RESTRICT =>
					-- board led_driver delete route_restrict 40 50 1
					case fields is
						when 7 =>
							-- delete a segment of route restrict
							delete_route_restrict (
								module_name 	=> module,
								point			=> type_point (set (
										x => to_distance (f (5)),
										y => to_distance (f (6)))),
								accuracy		=> to_distance (f (7)),
								
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_VIA_RESTRICT =>
					-- board led_driver delete via_restrict 40 50 1
					case fields is
						when 7 =>
							-- delete a segment of via restrict
							delete_via_restrict (
								module_name 	=> module,
								point			=> type_point (set (
										x => to_distance (f (5)),
										y => to_distance (f (6)))),
								accuracy		=> to_distance (f (7)),
								
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));

			end case;

		when VERB_DISPLAY => -- GUI related
			case noun is
				when NOUN_GRID => -- like "board led_driver display grid [on/off]"
					case fields is
						when 4 => display_grid; -- if status is omitted
						when 5 => display_grid (f (5));
						when 6 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_SILKSCREEN -- like "board led_driver display silkscreen top [on/off]"
					| NOUN_ASSY | NOUN_KEEPOUT | NOUN_STOP | NOUN_STENCIL | NOUN_ORIGINS =>
					case fields is
						when 5 => display_non_conductor_layer (noun, f (5)); -- if status is omitted
						when 6 => display_non_conductor_layer (noun, f (5), f (6));
						when 7 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;

				when NOUN_CONDUCTORS => -- like "board led_driver display conductors 2 [on/off]"
					case fields is
						when 5 => display_conductor_layer (f (5)); -- if status is omitted
						when 6 => display_conductor_layer (f (5), f (6));
						when 7 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;

				when NOUN_OUTLINE => -- like "board led_driver display outline [on/off]"
					case fields is
						when 4 => display_outline; -- if status is omitted
						when 5 => display_outline (f (5));
						when 6 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_RESTRICT => -- like "board led_driver display restrict route/via 2 [on/off]"
					case fields is
						when 6 => display_restrict_layer (f (5), f (6)); -- if status is omitted
						when 7 => display_restrict_layer (f (5), f (6), f (7));
						when 8 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;

				when NOUN_VIAS => -- like "board led_driver display vias [on/off]"
					case fields is
						--when 5 => display_vias (f (5)); -- if status is omitted
						--when 6 => display_vias (f (5), f (6));
						--when 7 .. count_type'last => too_long;
						when 4 => display_vias; -- if status is omitted
						when 5 => display_vias (f (5));
						when 6 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_DRAW =>
			case noun is
				when NOUN_OUTLINE =>
					declare
						shape : type_shape := to_shape (f (5));
					begin
						case shape is
							when LINE =>
								case fields is
									when 9 =>
										draw_outline_line (
											module_name 	=> module,
											line			=> (
												start_point	=> type_point (set (
													x => to_distance (f (6)),
													y => to_distance (f (7)))),
												end_point	=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
												locked		=> lock_status_default
												),
											log_threshold	=> log_threshold + 1
											);

									when 10 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;
								
							when ARC =>
								case fields is
									when 12 =>
										draw_outline_arc (
											module_name 	=> module,
											arc				=> (
												center		=> type_point (set (
													x => to_distance (f (6)),
													y => to_distance (f (7)))),
												start_point	=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
												end_point	=> type_point (set (
													x => to_distance (f (10)),
													y => to_distance (f (11)))),
												direction	=> to_direction (f (12)),
												locked	=> lock_status_default
												),

											log_threshold	=> log_threshold + 1
											);

									when 13 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;

							when CIRCLE =>
								case fields is
									when 8 =>
										draw_outline_circle (
											module_name 	=> module,
											circle			=> (
												center	=> type_point (set (
													x => to_distance (f (6)),
													y => to_distance (f (7)))),
												radius	=> to_distance (f (8)),
												locked	=> lock_status_default
												),

											log_threshold	=> log_threshold + 1
											);

									when 9 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;

						end case;
					end;

				when NOUN_SILKSCREEN =>
					declare
						use et_terminals.pac_shapes;
						shape : type_shape := to_shape (f (6));
					begin
						case shape is
							when LINE =>
								case fields is
									when 11 =>
										draw_silk_screen_line (
											module_name 	=> module,
											face			=> to_face (f (5)),
											line			=> (
														width		=> to_distance (f (7)),
														start_point	=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
														end_point	=> type_point (set (
															x => to_distance (f (10)),
															y => to_distance (f (11))))
														),

											log_threshold	=> log_threshold + 1
											);

									when 12 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;
								
							when ARC =>
								case fields is
									when 14 =>
										draw_silk_screen_arc (
											module_name 	=> module,
											face			=> to_face (f (5)),
											arc				=> (
														width	=> to_distance (f (7)),
														center	=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
														start_point	=> type_point (set (
															x => to_distance (f (10)),
															y => to_distance (f (11)))),
														end_point	=> type_point (set (
															x => to_distance (f (12)),
															y => to_distance (f (13)))),
														direction	=> to_direction (f (14))
														),

											log_threshold	=> log_threshold + 1
											);

									when 15 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;

							when CIRCLE =>
								case fields is
									when 10 =>

									-- The 7th field can either be a line width like 2.5 or a 
									-- fill style like CUTOUT or SOLID. HATCHED is not allowed here:
										if is_number (f (7)) then

											-- Circle is not filled and has a circumfence line width
											-- specified in field 7.
											draw_silk_screen_circle (
												module_name 	=> module,
												face			=> to_face (f (5)),
												circle			=> 
														(
														filled			=> NO,
														fill_style		=> fill_style_default, -- don't care here
														border_width	=> to_distance (f (7)),
														center			=> type_point (set (
																	x => to_distance (f (8)),
																	y => to_distance (f (9)))),
														radius			=> to_distance (f (10))
														),
												log_threshold	=> log_threshold + 1);
										else
											
											-- Circle is filled with the fill style specified in field 7:
											case to_fill_style (f (7)) is
-- CS
-- 														when CUTOUT =>
-- 													
-- 															draw_silk_screen_circle (
-- 																module_name 	=> module,
-- 																face			=> to_face (f (5)),
-- 																circle			=> 
-- 																			(
-- 																			filled		=> YES,
-- 																			fill_style	=> CUTOUT,
-- 																			center	=> type_point (set (
-- 																						x => to_distance (f (8)),
-- 																						y => to_distance (f (9)))),
-- 																			radius	=> to_distance (f (10))
-- 																			),
-- 																log_threshold	=> log_threshold + 1
-- 																);

												when SOLID =>
											
													draw_silk_screen_circle (
														module_name 	=> module,
														face			=> to_face (f (5)),
														circle			=> 
																	(
																	filled		=> YES,
																	fill_style	=> SOLID,
																	center	=> type_point (set (
																				x => to_distance (f (8)),
																				y => to_distance (f (9)))),
																	radius	=> to_distance (f (10))
																	),
														log_threshold	=> log_threshold + 1
														);

												when HATCHED =>
													command_incomplete (cmd);

											end case;
										end if;
											
									when 12 =>
										-- This is going to be a hatched circle.
										-- In this case the 7th field MUST be fill style HATCHED.
										if is_number (f (7)) then
											expect_fill_style (HATCHED, 7); -- error
										else
											case to_fill_style (f (7)) is
												when HATCHED =>
													draw_silk_screen_circle (
														module_name 	=> module,
														face			=> to_face (f (5)),
														circle			=> 
																(
																filled		=> YES,
																fill_style	=> HATCHED,
																center		=> type_point (set (
																			x => to_distance (f (8)),
																			y => to_distance (f (9)))),
																radius		=> to_distance (f (10)),
																hatching	=> (
																			line_width	=> to_distance (f (11)),
																			spacing		=> to_distance (f (12)),
																			others		=> <>
																			)
																),
														log_threshold	=> log_threshold + 1);

												when others =>
													expect_fill_style (HATCHED, 7);
											end case;
										end if;

									when 13 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;

										
							when others => null;
						end case;
					end;

				when NOUN_ASSY =>
					declare
						use et_terminals.pac_shapes;
						shape : type_shape := to_shape (f (6));
					begin
						case shape is
							when LINE =>
								case fields is
									when 11 =>
										draw_assy_doc_line (
											module_name 	=> module,
											face			=> to_face (f (5)),
											line			=> (
														width		=> to_distance (f (7)),
														start_point	=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
														end_point	=> type_point (set (
															x => to_distance (f (10)),
															y => to_distance (f (11))))
														),

											log_threshold	=> log_threshold + 1
											);

									when 12 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;
								
							when ARC =>
								case fields is
									when 14 =>
										draw_assy_doc_arc (
											module_name 	=> module,
											face			=> to_face (f (5)),
											arc				=> (
														width	=> to_distance (f (7)),
														center	=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
														start_point	=> type_point (set (
															x => to_distance (f (10)),
															y => to_distance (f (11)))),
														end_point	=> type_point (set (
															x => to_distance (f (12)),
															y => to_distance (f (13)))),
														direction	=> to_direction (f (14))
														),

											log_threshold	=> log_threshold + 1
											);

									when 15 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;

							when CIRCLE =>
								case fields is
									when 10 =>

									-- The 7th field can either be a line width like 2.5 or a 
									-- fill style like CUTOUT or SOLID. HATCHED is not allowed here:
										if is_number (f (7)) then

											-- Circle is not filled and has a circumfence line width
											-- specified in field 7.
											draw_assy_doc_circle (
												module_name 	=> module,
												face			=> to_face (f (5)),
												circle			=> 
														(
														filled			=> NO,
														fill_style		=> fill_style_default, -- don't care here
														border_width	=> to_distance (f (7)),
														center			=> type_point (set (
																	x => to_distance (f (8)),
																	y => to_distance (f (9)))),
														radius			=> to_distance (f (10))
														),
												log_threshold	=> log_threshold + 1);
										else
											
											-- Circle is filled with the fill style specified in field 7:
											case to_fill_style (f (7)) is
-- CS
-- 														when CUTOUT =>
-- 													
-- 															draw_assy_doc_circle (
-- 																module_name 	=> module,
-- 																face			=> to_face (f (5)),
-- 																circle			=> 
-- 																			(
-- 																			filled		=> YES,
-- 																			fill_style	=> CUTOUT,
-- 																			center	=> type_point (set (
-- 																						x => to_distance (f (8)),
-- 																						y => to_distance (f (9)))),
-- 																			radius	=> to_distance (f (10))
-- 																			),
-- 																log_threshold	=> log_threshold + 1
-- 																);

												when SOLID =>
											
													draw_assy_doc_circle (
														module_name 	=> module,
														face			=> to_face (f (5)),
														circle			=> 
																	(
																	filled		=> YES,
																	fill_style	=> SOLID,
																	center	=> type_point (set (
																				x => to_distance (f (8)),
																				y => to_distance (f (9)))),
																	radius	=> to_distance (f (10))
																	),
														log_threshold	=> log_threshold + 1
														);

												when HATCHED =>
													command_incomplete (cmd);

											end case;
										end if;
											
									when 12 =>
										-- This is going to be a hatched circle.
										-- In this case the 7th field MUST be fill style HATCHED.
										if is_number (f (7)) then
											expect_fill_style (HATCHED, 7); -- error
										else
											case to_fill_style (f (7)) is
												when HATCHED =>
													draw_assy_doc_circle (
														module_name 	=> module,
														face			=> to_face (f (5)),
														circle			=> 
																(
																filled		=> YES,
																fill_style	=> HATCHED,
																center		=> type_point (set (
																			x => to_distance (f (8)),
																			y => to_distance (f (9)))),
																radius		=> to_distance (f (10)),

																hatching	=> (
																			line_width	=> to_distance (f (11)),
																			spacing		=> to_distance (f (12)),
																			others		=> <>
																			)
																),
														log_threshold	=> log_threshold + 1);

												when others =>
													expect_fill_style (HATCHED, 7);
											end case;
										end if;

									when 13 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;

										
							when others => null;
						end case;
					end;

				when NOUN_KEEPOUT =>
					draw_keepout;
					
				when NOUN_ROUTE_RESTRICT =>
					draw_route_restrict;

				when NOUN_STENCIL =>
					draw_stencil;
					
				when NOUN_STOP =>
					draw_stop_mask;

				when NOUN_VIA_RESTRICT =>
					draw_via_restrict;
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_EXECUTE =>
			case noun is
				when NOUN_SCRIPT =>
					case fields is
						when 5 => 
							execute_nested_script (
								file			=> f (5),
								log_threshold	=> log_threshold + 1);

						when 6 .. count_type'last => too_long;								
						when others => command_incomplete (cmd);
					end case;
						
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_EXIT | VERB_QUIT => terminate_main;
			
		when VERB_FLIP =>
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 6 =>
							flip_device (
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								face			=> to_face  (f (6)),  -- top/bottom
								log_threshold	=> log_threshold + 1
								);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_POSITION => -- GUI related
			case noun is 
				when NOUN_CURSOR =>
					case fields is
						when 7 => position_cursor; -- position cursor absolute/relative 25 30
						when 8 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_RENAME =>
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 6 => rename_device; -- board led_driver renames device FD1 FD3

						when 7 .. count_type'last => too_long;
						
						when others => command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_ROUTE =>
			case noun is
				when NOUN_FREETRACK =>
					declare
						shape : type_track_shape := to_shape (f (6));
					begin
						case shape is
							when LINE =>
								case fields is
									when 11 =>
										-- draw a freetrack
										draw_track_line (
											module_name 	=> module,
											net_name		=> to_net_name (""),
											line	=> (
												width		=> to_distance (f (7)),
												start_point	=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
												end_point	=> type_point (set (
													x => to_distance (f (10)),
													y => to_distance (f (11)))),
												layer		=> to_signal_layer (f (5))
												),
											log_threshold	=> log_threshold + 1
											);

									when 12 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;
								
							when ARC =>
								case fields is
									when 14 =>
										-- draw a freetrack
										draw_track_arc (
											module_name 	=> module,
											arc			=> (
												layer			=> to_signal_layer (f (5)),
												width			=> to_distance (f (7)),
												center			=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
												start_point		=> type_point (set (
													x => to_distance (f (10)),
													y => to_distance (f (11)))),
												end_point		=> type_point (set (
													x => to_distance (f (12)),
													y => to_distance (f (13)))),
												direction	=> to_direction (f (14))
													),
											net_name		=> to_net_name (""),

											log_threshold	=> log_threshold + 1
											);
										
									when 15 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;

						end case;
					end;

				when NOUN_NET =>
					route_net;
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_RIPUP =>
			case noun is
				when NOUN_FREETRACK =>
					case fields is
						when 8 =>
							-- ripup a segment of a freetrack
							ripup_track_segment (
								module_name 	=> module,
								net_name		=> to_net_name (""),
								layer			=> to_signal_layer (f (5)),
								point			=> type_point (set (
										x => to_distance (f (6)),
										y => to_distance (f (7)))),
								accuracy		=> to_distance (f (8)),
								
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last =>
							command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;

				when NOUN_NET =>
					case fields is
						when 9 =>
							-- ripup a segment of a named track
							ripup_track_segment (
								module_name 	=> module,
								net_name		=> to_net_name (f (5)),
								layer			=> to_signal_layer (f (6)),
								point			=> type_point (set (
										x => to_distance (f (7)),
										y => to_distance (f (8)))),
								accuracy		=> to_distance (f (9)),
								
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last =>
							command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));

			end case;
			
		when VERB_ROTATE =>
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 7 =>
							rotate_device (
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								coordinates		=> to_coordinates (f (6)),  -- relative/absolute
								rotation		=> to_rotation (f (7)),
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last =>
							command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_MAKE =>
			case noun is
				when NOUN_PNP =>
					case fields is
						when 4 =>
							make_pick_and_place 
								(
								module_name 	=> module,
								log_threshold	=> log_threshold + 1);

						when 5 .. count_type'last =>
							command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_MOVE =>
			case noun is
				when NOUN_BOARD =>
					case fields is
						when 7 => -- board led_driver move board absolute 20 50
							move_board (
								module_name 	=> module,
								coordinates		=> to_coordinates (f (5)),  -- relative/absolute
								point			=> type_point (set (
													x => to_distance (f (6)),
													y => to_distance (f (7)))),
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last =>
							command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;
					
				when NOUN_DEVICE =>
					case fields is
						when 8 =>
							move_device (
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								coordinates		=> to_coordinates (f (6)),  -- relative/absolute
								point			=> type_point (set (
													x => to_distance (f (7)),
													y => to_distance (f (8)))),
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last =>
							command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;

				when NOUN_SUBMODULE =>
					case fields is
						when 8 =>
							move_submodule (
								module_name 	=> module,
								instance		=> et_general.to_instance_name (f (5)), -- OSC1
								coordinates		=> to_coordinates (f (6)),  -- relative/absolute
								point			=> type_point (set (
													x => to_distance (f (7)),
													y => to_distance (f (8)))),
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last =>
							command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_SET =>
			case noun is
				when NOUN_GRID =>
					case fields is
						-- board led_driver set grid 0.5 0.5
						when 6 =>
							set_grid (
								module_name 	=> module,
								grid			=> (
										x => to_distance (f (5)),
										y => to_distance (f (6))),
								log_threshold	=> log_threshold + 1);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));

			end case;

-- 			when VERB_SHOW => -- GUI related
-- 				case noun is
-- 					when NOUN_DEVICE =>
-- 						case fields is
-- 							when 5 => null; -- CS
-- 							when 6 .. count_type'last => too_long;
-- 							when others => command_incomplete (cmd);
-- 						end case;
-- 						
-- 					when others => invalid_noun (to_string (noun));
-- 				end case;

		when VERB_ZOOM => -- GUI related
			case noun is
				when NOUN_FIT => -- zoom fit
					case fields is
						when 4 => 
							log (text => "zoom to fit", level => log_threshold + 1);
							scale_to_fit (canvas);

						when 5 .. count_type'last => too_long;

						when others => command_incomplete (cmd);
					end case;

				when NOUN_LEVEL => -- zoom level 3
					case fields is
						when 4 => 
							set_scale (f (5));

						when 6 .. count_type'last => too_long;

						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_CENTER => -- zoom center 10 10
					case fields is
						when 6 =>  -- zoom center 10 10
							zoom_center;

						when 7 =>  -- zoom center 10 10 0.5
							zoom_center;
							set_scale (f (7));

						when 8 .. count_type'last => too_long;

						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
	end case;

	return exit_code;

	exception when event: others => 
	
		log (ERROR, "board command " & enclose_in_quotes (to_string (cmd)) &
				" invalid !", console => true);

		log (text => ada.exceptions.exception_information (event), console => true);		

	return ERROR;
	
end board_cmd;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
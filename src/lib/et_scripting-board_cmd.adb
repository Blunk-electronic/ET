------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          SCRIPTING IN BOARD                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
--   ToDo:
--   - command to define a global cutout area

with et_board_shapes_and_text;
with et_drills;
with et_modes.board;
with et_canvas_board_devices;
with et_canvas_board_texts;
with et_canvas_board_vias;
with et_design_rules;				use et_design_rules;
with et_conductor_polygons;			use et_conductor_polygons;

separate (et_scripting)
	
procedure board_cmd (
	module_cursor	: in pac_generic_modules.cursor;
	cmd_in			: in type_fields_of_line; -- "board tree_1 draw silk top line 2.5 0 0 160 0"
	log_threshold	: in type_log_level)
is
	use et_board_ops;
	use et_packages;
	use et_board_shapes_and_text.pac_shapes;
	--use et_terminals.pac_shapes;
	use et_drills;
	use et_vias;
	use et_pcb;
	use et_pcb_coordinates;
	use et_pcb_coordinates.pac_geometry_brd;
	use et_pcb_stack;
	use et_canvas_board.pac_canvas;
	use et_display.board;
	use et_modes.board;
	
	module	: pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
	
	function f (place : in count_type) return string is begin
		return get_field (single_cmd_status.cmd, place);
	end;

	function fields return count_type is begin
		return field_count (single_cmd_status.cmd);
	end;

	procedure too_long is begin -- CS use it more often
		command_too_long (single_cmd_status.cmd, fields - 1);
	end;

	procedure command_incomplete is begin
		if runmode /= MODE_HEADLESS and cmd_entry_mode = SINGLE_CMD then
			single_cmd_status.complete := false;
		else
			raise exception_command_incomplete with "command not complete";
		end if;
	end command_incomplete;

	
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
		status	: in string := "") 
	is 
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
		layer	: in type_noun;
		face	: in string; -- top/bottom
		status	: in string := "") 
	is
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
		status	: in string := "")
	is 
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
	   status	: in string := "") 
	is 
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
		status	: in string := "")
	is 
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

	procedure draw_outline is
		-- Extract from the given command the polygon arguments (everything after "outline"):
		arguments : constant type_fields_of_line := remove (single_cmd_status.cmd, 1, 4);

		-- Build a basic polygon from the arguments:
		p0 : constant type_polygon_base'class := to_polygon (arguments);
	begin
		draw_outline (module, type_polygon (p0), log_threshold + 1);
	end draw_outline;

	procedure draw_hole is
		-- Extract from the given command the polygon arguments (everything after "hole"):
		arguments : constant type_fields_of_line := remove (single_cmd_status.cmd, 1, 4);

		-- Build a basic polygon from the arguments:
		p0 : constant type_polygon_base'class := to_polygon (arguments);
	begin
		draw_hole (module, type_polygon (p0), log_threshold + 1);
	end draw_hole;

	procedure delete_outline_segment is begin
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

			when 8 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
				
			when others => command_incomplete;
		end case;
	end delete_outline_segment;

	procedure delete_hole_segment is begin
		case fields is
			when 7 =>
				-- delete a segment of a hole
				delete_hole (
					module_name 	=> module,
					point			=> type_point (set (
							x => to_distance (f (5)),
							y => to_distance (f (6)))),
					accuracy		=> to_distance (f (7)),
					
					log_threshold	=> log_threshold + 1);

			when 8 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
				
			when others => command_incomplete;
		end case;
	end delete_hole_segment;

	
	procedure draw_silkscreen is
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
						command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
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
						command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
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
									command_incomplete;

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
						command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
				end case;

						
			when others => null;
		end case;
	end draw_silkscreen;

	procedure draw_assy_doc is
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
						command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
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
						command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
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
									command_incomplete;

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
						command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
				end case;

						
			when others => null;
		end case;
	end draw_assy_doc;
	
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

					when 11 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

					when 14 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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
							
					when 12 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

					when 11 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

					when 14 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

					when 11 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
					
					when others => command_incomplete;
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

					when 11 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

					when 14 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

					when 11 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
					
					when others => command_incomplete;
				end case;
						
			when others => null;
		end case;
	end draw_via_restrict;

	procedure draw_stop_mask is
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

					when 12 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

					when 15 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

								when HATCHED => command_incomplete;

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

					when 13 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
				end case;

						
			when others => null;
		end case;
	end draw_stop_mask;

	procedure draw_stencil is
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

					when 12 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

					when 15 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
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

								when HATCHED => command_incomplete;

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

					when 13 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others => command_incomplete;
				end case;
						
			when others => null;
		end case;
	end draw_stencil;

	procedure place_text is
		text			: type_text_with_content;
		pos_xy			: type_point;
		rotation		: type_rotation;
		layer_category	: type_layer_category;
		signal_layer	: type_signal_layer;
	begin
		-- board demo place text outline 0.15 1 140 100 0 "SILKSCREEN"
		-- board demo place text silkscreen top 0.15 1 140 100 0 "SILKSCREEN"
		-- board demo place text conductor  5   0.15 1 140 100 0 "L1"
		case fields is
			when 11 =>
				layer_category := to_layer_category (f (5));
				
				text.line_width := to_distance (f (6)); -- 0.15
				text.size := to_distance (f (7)); -- 1
				
				pos_xy := type_point (set (
							x => to_distance (f (8)), -- 140
							y => to_distance (f (9)))); -- 100

				rotation := to_rotation (f (10)); -- 0
				text.position := type_position (to_position (pos_xy, rotation));
				
				text.content := to_content (f (11));
				-- CS check length
				if characters_valid (text.content) then

					if layer_category in type_layer_category_outline then

						place_text_in_outline_layer (
							module_cursor 	=> module_cursor,
							layer_category	=> layer_category,
							text			=> text,
							log_threshold	=> log_threshold + 1);

					else
						raise semantic_error_1 with
							"ERROR: Text not allowed in this layer category !";
						-- CS should never happen
					end if;

				else
					raise syntax_error_1 with
						"ERROR: Invalid character in text !";
					-- CS show invalid character and its position
				end if;
				
			when 12 =>
				layer_category := to_layer_category (f (5));
				
				text.line_width := to_distance (f (7)); -- 0.15
				text.size := to_distance (f (8)); -- 1
				
				pos_xy := type_point (set (
							x => to_distance (f (9)), -- 140
							y => to_distance (f (10)))); -- 100

				rotation := to_rotation (f (11)); -- 0
				text.position := type_position (to_position (pos_xy, rotation));
				
				text.content := to_content (f (12));
				-- CS check length
				if characters_valid (text.content) then

					if layer_category in type_layer_category_non_conductor then

						place_text_in_non_conductor_layer (
							module_cursor 	=> module_cursor,
							layer_category	=> layer_category,
							face			=> to_face (f (6)),
							text			=> text,
							log_threshold	=> log_threshold + 1);

					elsif layer_category in type_layer_category_conductor then
						signal_layer := to_signal_layer (f (6));  -- 5 
						
						place_text_in_conductor_layer (
							module_cursor 	=> module_cursor,
							layer_category	=> layer_category,
							text			=> ((text with signal_layer)),
							log_threshold	=> log_threshold + 1);

					else
						raise semantic_error_1 with
							"ERROR: Text not allowed in this layer category !";
						-- CS should never happen
					end if;

				else
					raise syntax_error_1 with
						"ERROR: Invalid character in text !";
					-- CS show invalid character and its position
				end if;
					
			when 13 .. count_type'last => too_long;
				
			when others => command_incomplete;
		end case;
	end place_text;

	-- Parses a command like "board demo set via restring inner/outer 0.2"
	-- or "board demo set via restring inner 0.2" and sets the value
	-- for user specific via drill or restring.
	-- If the field for the value contains the keyword "dru" instead of 0.2
	-- then the user specific value is deactivated so that the value
	-- is taken from the DRU settings.
	procedure set_via_properties is
		kw_drill	: constant string := "drill";
		kw_restring	: constant string := "restring";
		kw_inner	: constant string := "inner";
		kw_outer	: constant string := "outer";
		kw_dru		: constant string := "dru";

		procedure expect_keywords is begin
			raise syntax_error_1 with 
				"ERROR: Expect keyword "
				& enclose_in_quotes (kw_drill) & " or "
				& enclose_in_quotes (kw_restring) 
				& " after " & enclose_in_quotes (to_lower (to_string (noun))) & " !";
		end expect_keywords;

		use pac_generic_modules;
		use et_schematic;
		
		procedure deactivate_drill (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.vias.drill.active := false;
		end deactivate_drill;

		procedure activate_drill (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.vias.drill.active := true;
			module.board.user_settings.vias.drill.size := to_distance (f (6));
		end activate_drill;

		procedure deactivate_inner_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.vias.restring_inner.active := false;
		end deactivate_inner_restring;

		procedure activate_inner_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.vias.restring_inner.active := true;
			module.board.user_settings.vias.restring_inner.width := to_distance (f (6));
		end activate_inner_restring;

		procedure deactivate_outer_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.vias.restring_outer.active := false;
		end deactivate_outer_restring;

		procedure activate_outer_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.vias.restring_outer.active := true;
			module.board.user_settings.vias.restring_outer.width := to_distance (f (6));
		end activate_outer_restring;
		
	begin -- set_via_properties
		case fields is
			when 6 => 
				-- board demo set via drill 0.3/dru
				if f (5) = kw_drill then
					if f (6) = kw_dru then
						update_element (generic_modules, module_cursor, deactivate_drill'access);
					else
						update_element (generic_modules, module_cursor, activate_drill'access);

						-- CS validate against dru settings
					end if;
				else
					expect_keywords;
				end if;

			when 7 => 
				-- board demo set via restring inner/outer 0.2
				if f (5) = kw_restring then

					-- board demo set via restring inner 0.2
					-- board demo set via restring inner dru
					if f (6) = kw_inner then
						if f (7) = kw_dru then
							update_element (generic_modules, module_cursor, deactivate_inner_restring'access);
						else
							update_element (generic_modules, module_cursor, activate_inner_restring'access);

							-- CS validate against dru settings	
						end if;

					-- board demo set via restring outer 0.2
					-- board demo set via restring outer dru
					elsif f (6) = kw_outer then
						if f (7) = kw_dru then
							update_element (generic_modules, module_cursor, deactivate_outer_restring'access);
						else
							update_element (generic_modules, module_cursor, activate_outer_restring'access);

							-- CS validate against dru settings
						end if;
						
					else
						raise syntax_error_1 with
							"ERROR: Expect keywords " 
							& enclose_in_quotes (kw_inner) & " or "
							& enclose_in_quotes (kw_outer) 
							& " after keyword " & enclose_in_quotes (kw_restring) & " !";
					
					end if;
				else
					expect_keywords;
				end if;
				
			when 8 .. count_type'last => too_long;

			when others => command_incomplete;
		end case;

	end set_via_properties;

	-- This procedure builds the final via and calls et_board_ops.place_via
	-- accordingly. User specific settings are taken into account.
	-- CS: Take into account class settings (via drill size).
	procedure place_via is
		net_name		: pac_net_name.bounded_string;
		drill			: type_drill;
		restring_outer	: type_restring_width;
		restring_top	: type_restring_width;
		restring_bottom	: type_restring_width;
		restring_inner	: type_restring_width;
		buried_layers	: type_buried_layers;
		lower_layer		: type_via_layer;
		upper_layer		: type_via_layer;

		procedure set_net_name is begin
			-- CS check net name: characters, lenth, existence of net
			net_name := to_net_name (f (5));
		end set_net_name;

		procedure set_position is begin
			drill.position := type_point (set (
				x => to_distance (f (6)), -- 10
				y => to_distance (f (7)))); -- 14

			-- CS check position: must be inside board area
		end set_position;

		keyword_buried	: constant string := "buried";
		keyword_blind	: constant string := "blind";
		keyword_top		: constant string := "top";
		keyword_bottom	: constant string := "bottom";

		procedure through is
			via : type_via (THROUGH);
		begin
			via := (drill with
				category		=> THROUGH,
				restring_inner	=> restring_inner,
				restring_outer	=> restring_outer);
					
			et_board_ops.place_via (module_cursor, net_name, via, log_threshold + 1);
		end through;

		procedure blind_top is
			via : type_via (BLIND_DRILLED_FROM_TOP);
		begin
			via := (drill with
				category		=> BLIND_DRILLED_FROM_TOP,
				restring_inner	=> restring_inner,
				restring_top	=> restring_top,
				lower			=> lower_layer);
					
			et_board_ops.place_via (module_cursor, net_name, via, log_threshold + 1);
		end blind_top;

		procedure blind_bottom is
			via : type_via (BLIND_DRILLED_FROM_BOTTOM);
		begin
			via := (drill with
				category		=> BLIND_DRILLED_FROM_BOTTOM,
				restring_inner	=> restring_inner,
				restring_bottom	=> restring_bottom,
				upper			=> upper_layer);
					
			et_board_ops.place_via (module_cursor, net_name, via, log_threshold + 1);
		end blind_bottom;

		procedure buried is
			via : type_via (BURIED);
		begin
			via := (drill with
				category		=> BURIED,
				restring_inner	=> restring_inner,
				layers			=> buried_layers);
					
			et_board_ops.place_via (module_cursor, net_name, via, log_threshold + 1);
		end buried;

		use et_design_rules;
		rules : constant type_design_rules := get_pcb_design_rules (module_cursor);

		-- get the user specific settings of the board
		settings : constant type_user_settings := get_user_settings (module_cursor);
		
	begin -- place_via
		-- Set the drill size and restring according to a user specific values:
		-- If user has not specified defaults, use values given in DRU data set:

		-- set drill size:
		if settings.vias.drill.active then
			drill.diameter	:= settings.vias.drill.size;
		else
			drill.diameter	:= rules.sizes.drills;
		end if;

		-- CS: take minimum drill diameter as defined in net class into account
		-- Requres a command like "set via drill class"
		
		-- set outer restring:
		if settings.vias.restring_outer.active then
			restring_outer	:= settings.vias.restring_outer.width;
		else
			restring_outer	:= auto_set_restring (OUTER, drill.diameter);
		end if;
		
		restring_top	:= restring_outer; -- for blind via drilled from top
		restring_bottom	:= restring_outer; -- for blind via drilled from bottom

		-- set inner restring:
		if settings.vias.restring_inner.active then
			restring_inner	:= settings.vias.restring_inner.width;
		else
			restring_inner	:= auto_set_restring (INNER, drill.diameter, rules.sizes.restring.delta_size);
		end if;
		
		case fields is
			when 7 => 
				-- board demo place via RESET_N 10 14
				set_net_name;
				set_position;
				through;
				
			when 10 =>
				if f (8) = keyword_buried then

					-- board demo place via RESET_N 10 14 buried 2 15
					set_net_name;
					set_position;
					buried_layers := to_buried_layers (
								upper	=> f (9), 
								lower	=> f (10),
								bottom	=> deepest_conductor_layer (module_cursor));
					buried;

				elsif f (8) = keyword_blind then

					-- board demo place via RESET_N 10 14 blind top 5
					-- board demo place via RESET_N 10 14 blind bottom 2
					set_net_name;
					set_position;

					if f (9) = keyword_top then
						lower_layer := to_signal_layer (f (10));
						blind_top;
						
					elsif f (9) = keyword_bottom then
						upper_layer := to_signal_layer (f (10));
						blind_bottom;
						
					else
						raise syntax_error_1 with 
							"ERROR: Expect keywords " 
							& enclose_in_quotes (keyword_top)
							& " or " 
							& enclose_in_quotes (keyword_bottom)
							& " after keyword " 
							& enclose_in_quotes (keyword_blind)
							& " !";							
					end if;
						
				else
					raise syntax_error_1 with 
						"ERROR: Expect keywords " & enclose_in_quotes (keyword_blind)
						& " or " & enclose_in_quotes (keyword_buried)
						& " after y position !";
				end if;
				
			when 11 .. count_type'last => too_long;
				
			when others => command_incomplete;
		end case;

	end place_via;



-- ROUTE / TRACK / POLYGON

	-- Applies to polygons (or fill zones) in conductor layers only.
	-- Parses a command like "board demo set polygon fill solid/hatched"
	-- or "board demo set polygon isolaton 0.4" and sets the value
	-- in user specific settings..
	procedure set_polygon_properties is
		kw_fill			: constant string := "fill";
		kw_width		: constant string := "width";
		kw_isolation	: constant string := "isolation";
		kw_priority		: constant string := "priority";
		
		kw_easing		: constant string := "easing";
		kw_style		: constant string := "style";
		kw_radius		: constant string := "radius";
		
		kw_hatching		: constant string := "hatching";
		kw_border		: constant string := "border";
		kw_spacing		: constant string := "spacing";
		
		kw_connection	: constant string := "connection";
		kw_thermal		: constant string := "thermal";
		kw_gap			: constant string := "gap";
		
		use pac_generic_modules;
		use et_schematic;

		comma : constant character := ',';
		
		procedure expect_keywords is begin
			raise syntax_error_1 with 
				"ERROR: Expect keyword "
				& enclose_in_quotes (kw_width) & comma
				& enclose_in_quotes (kw_connection) & comma
				& enclose_in_quotes (kw_priority) & comma
				& enclose_in_quotes (kw_hatching) & comma
				& enclose_in_quotes (kw_thermal) & comma
				& enclose_in_quotes (kw_easing) & " or "
				& enclose_in_quotes (kw_isolation) 
				& " after keyword " & enclose_in_quotes (to_lower (to_string (noun))) & " !";
		end expect_keywords;


		procedure set_fill_style (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.fill_style := to_fill_style (f (6));
		end set_fill_style;

		procedure set_min_width (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.min_width := to_distance (f (6));
		end set_min_width;

		procedure set_iso (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.isolation := to_distance (f (6));
		end set_iso;

		procedure set_priority (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.priority_level := to_polygon_priority (f (6));
		end set_priority;

		procedure set_easing_style (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.easing.style := to_easing_style (f (7));
		end set_easing_style;

		procedure set_easing_radius (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.easing.radius := to_distance (f (7));
		end set_easing_radius;	

		procedure set_connection (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.connection := to_pad_connection (f (6));
		end set_connection;	

		procedure set_hatching_line_width (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.hatching.line_width := to_distance (f (7));
		end set_hatching_line_width;	

		procedure set_hatching_border_width (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.hatching.border_width := to_distance (f (7));
		end set_hatching_border_width;	

		procedure set_hatching_spacing (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.hatching.spacing := to_distance (f (7));
		end set_hatching_spacing;	

		procedure set_thermal_width (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.thermal.width := to_distance (f (7));
		end set_thermal_width;	

		procedure set_thermal_gap (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.user_settings.polygons_conductor.thermal.gap := to_distance (f (7));
		end set_thermal_gap;	
		
	begin -- set_polygon_properties
		case fields is
			when 6 => 
				-- board demo set polygon fill solid/hatched
				if f (5) = kw_fill then
					update_element (generic_modules, module_cursor, set_fill_style'access);

				-- board demo set polygon width 0.25
				elsif f (5) = kw_width then
					update_element (generic_modules, module_cursor, set_min_width'access);

				-- board demo set polygon isolaton 0.4
				elsif f (5) = kw_isolation then
					update_element (generic_modules, module_cursor, set_iso'access);
					
				-- board demo set polygon priority 2
				elsif f (5) = kw_priority then
					update_element (generic_modules, module_cursor, set_priority'access);

				-- board demo set polygon connection thermal/solid
				elsif f (5) = kw_connection then
					update_element (generic_modules, module_cursor, set_connection'access);
					
				else
					expect_keywords;
				end if;

			when 7 =>
				-- board demo set polygon easing style none/chamfer/fillet
				if f (5) = kw_easing then

					if f (6) = kw_style then
						update_element (generic_modules, module_cursor, set_easing_style'access);

					elsif f (6) = kw_radius then
						update_element (generic_modules, module_cursor, set_easing_radius'access);

					else
						raise syntax_error_1 with
							"ERROR: Expect keywords " 
							& enclose_in_quotes (kw_style) & " or "
							& enclose_in_quotes (kw_radius) 
							& " after keyword " & enclose_in_quotes (kw_easing) & " !";
					end if;

				-- board demo set polygon hatching width/border/spacing 0.3
				elsif f (5) = kw_hatching then

					if f (6) = kw_width then
						update_element (generic_modules, module_cursor, set_hatching_line_width'access);

					elsif f (6) = kw_border then
						update_element (generic_modules, module_cursor, set_hatching_border_width'access);

					elsif f (6) = kw_spacing then
						update_element (generic_modules, module_cursor, set_hatching_spacing'access);
						
					else
						raise syntax_error_1 with
						"ERROR: Expect keywords " 
							& enclose_in_quotes (kw_width) & comma
							& enclose_in_quotes (kw_border) & " or "
							& enclose_in_quotes (kw_spacing) 
							& " after keyword " & enclose_in_quotes (kw_hatching) & " !";
					end if;

				-- board demo set polygon thermal width/gap 0.3
				elsif f (5) = kw_thermal then

					if f (6) = kw_width then
						update_element (generic_modules, module_cursor, set_thermal_width'access);

					elsif f (6) = kw_gap then
						update_element (generic_modules, module_cursor, set_thermal_gap'access);

					-- CS technology ?
						
					else
						raise syntax_error_1 with
						"ERROR: Expect keywords " 
							& enclose_in_quotes (kw_width) & " or "
							& enclose_in_quotes (kw_gap) 
							& " after keyword " & enclose_in_quotes (kw_thermal) & " !";
					end if;

				else
					expect_keywords;
				end if;
				
			when 8 .. count_type'last => too_long;

			when others => command_incomplete;
				
		end case;
	end set_polygon_properties;
	
	type type_track_shape is (LINE, ARC, POLYGON);
	-- CS circular tracks are currently not supported

	procedure route_freetrack is
		shape : constant type_track_shape := type_track_shape'value (f (6));

		-- get the user specific settings of the board
		settings : constant type_user_settings := get_user_settings (module_cursor);
		

		procedure make_polygon is
			-- Extract from the given command the polygon arguments (everything after "polygon"):
			arguments : constant type_fields_of_line := remove (single_cmd_status.cmd, 1, 6);
			
			ps : type_polygon_conductor_solid_floating;
			ph : type_polygon_conductor_hatched_floating;

			-- Build a basic polygon from the arguments:
			p : constant type_polygon_base'class := to_polygon (arguments);
		begin -- make_polygon
			case settings.polygons_conductor.fill_style is
				when SOLID =>
			
					ps := (type_polygon_base (p) with 
						fill_style	=> SOLID,
						width_min	=> settings.polygons_conductor.min_width,
						isolation	=> settings.polygons_conductor.isolation,
						properties	=> (
							layer 			=> to_signal_layer (f (5)),
							priority_level	=> settings.polygons_conductor.priority_level,
							fill_lines		=> no_fill_lines),
						
						easing		=> settings.polygons_conductor.easing);

					place_polygon_conductor (module_cursor, ps, log_threshold + 1);

					
				when HATCHED =>

					ph := (type_polygon_base (p) with 
						fill_style	=> HATCHED,
						hatching	=> settings.polygons_conductor.hatching,
						width_min	=> settings.polygons_conductor.min_width,
						isolation	=> settings.polygons_conductor.isolation,
						properties	=> (
							layer 			=> to_signal_layer (f (5)),
							priority_level	=> settings.polygons_conductor.priority_level,
							fill_lines		=> no_fill_lines),
						
						easing		=> settings.polygons_conductor.easing);

					place_polygon_conductor (module_cursor, ph, log_threshold + 1);
					
			end case;
		end make_polygon;
		
	begin -- route_freetrack
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
						command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
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
						command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
				end case;

			when POLYGON =>
				case fields is
					-- The polygon command is very long. The following example spreads across
					-- several lines:
					--  board led_driver route freetrack 1 polygon /
					--  line 0 0 100 0 /
					--  line 100 0 100 100 / 
					--  arc 50 100 100 100 0 100 ccw / 
					--  line 0 100 0 0
					when 5 .. count_type'last =>
						make_polygon;

					when others =>
						command_incomplete;
				end case;
			end case;
	end route_freetrack;
	
	procedure route_net is 
		use et_terminals;
		shape : constant type_track_shape := type_track_shape'value (f (7));

		-- get the user specific settings of the board
		settings : constant type_user_settings := get_user_settings (module_cursor);
		
		procedure make_polygon is
			-- Extract from the given command the polygon arguments (everything after "polygon"):
			arguments : constant type_fields_of_line := remove (single_cmd_status.cmd, 1, 7);

			-- Build a basic polygon from the arguments:
			p0 : constant type_polygon_base'class := to_polygon (arguments);

			procedure make_solid_thermal is
				p1 : type_polygon_conductor_solid;
				p2 : type_polygon_conductor_route_solid (connection => THERMAL);
			begin
				p1 := (type_polygon_base (p0) with 
					fill_style	=> SOLID,
					width_min	=> settings.polygons_conductor.min_width,
					isolation	=> settings.polygons_conductor.isolation,					
					easing		=> settings.polygons_conductor.easing);

				p2 := (p1 with
					connection	=> THERMAL,
					thermal		=> settings.polygons_conductor.thermal,
					properties	=> (
						layer 			=> to_signal_layer (f (6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						fill_lines		=> no_fill_lines));
					   
				place_polygon_conductor (
					module_cursor	=> module_cursor,
					polygon			=> p2,
					net_name		=> to_net_name (f (5)),
					log_threshold	=> log_threshold + 1);
				
			end make_solid_thermal;

			procedure make_solid_solid is 
				p1 : type_polygon_conductor_solid;
				p2 : type_polygon_conductor_route_solid (connection => SOLID);
			begin
				p1 := (type_polygon_base (p0) with 
					fill_style	=> SOLID,
					width_min	=> settings.polygons_conductor.min_width,
					isolation	=> settings.polygons_conductor.isolation,					
					easing		=> settings.polygons_conductor.easing);

				p2 := (p1 with
					connection	=> SOLID,
					technology	=> SMT_AND_THT, -- CS settings.polygons_conductor.technology,
					properties	=> (
						layer 			=> to_signal_layer (f (6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						fill_lines		=> no_fill_lines));
					   
				place_polygon_conductor (
					module_cursor	=> module_cursor,
					polygon			=> p2,
					net_name		=> to_net_name (f (5)),
					log_threshold	=> log_threshold + 1);

			end make_solid_solid;

			procedure make_hatched_thermal is
				p1 : type_polygon_conductor_hatched;
				p2 : type_polygon_conductor_route_hatched (connection => THERMAL);
			begin
				p1 := (type_polygon_base (p0) with 
					fill_style	=> HATCHED,
					hatching	=> settings.polygons_conductor.hatching,
					width_min	=> settings.polygons_conductor.min_width,
					isolation	=> settings.polygons_conductor.isolation,					
					easing		=> settings.polygons_conductor.easing);

				p2 := (p1 with
					connection	=> THERMAL,
					thermal		=> settings.polygons_conductor.thermal,
					properties	=> (
						layer 			=> to_signal_layer (f (6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						fill_lines		=> no_fill_lines));
					   
				place_polygon_conductor (
					module_cursor	=> module_cursor,
					polygon			=> p2,
					net_name		=> to_net_name (f (5)),
					log_threshold	=> log_threshold + 1);
				
			end make_hatched_thermal;

			procedure make_hatched_solid is
				p1 : type_polygon_conductor_hatched;
				p2 : type_polygon_conductor_route_hatched (connection => SOLID);
			begin
				p1 := (type_polygon_base (p0) with 
					fill_style	=> HATCHED,
					hatching	=> settings.polygons_conductor.hatching,
					width_min	=> settings.polygons_conductor.min_width,
					isolation	=> settings.polygons_conductor.isolation,					
					easing		=> settings.polygons_conductor.easing);

				p2 := (p1 with
					connection	=> SOLID,
					technology	=> SMT_AND_THT, -- CS settings.polygons_conductor.technology,
					properties	=> (
						layer 			=> to_signal_layer (f (6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						fill_lines		=> no_fill_lines));
					   
				place_polygon_conductor (
					module_cursor	=> module_cursor,
					polygon			=> p2,
					net_name		=> to_net_name (f (5)),
					log_threshold	=> log_threshold + 1);
				
			end make_hatched_solid;
			
		begin -- make_polygon
			case settings.polygons_conductor.fill_style is
				when SOLID =>
					case settings.polygons_conductor.connection is
						when THERMAL	=> make_solid_thermal;
						when SOLID		=> make_solid_solid;
					end case;
					
				when HATCHED =>
					case settings.polygons_conductor.connection is
						when THERMAL	=> make_hatched_thermal;
						when SOLID		=> make_hatched_solid;
					end case;
					
			end case;
		end make_polygon;

	begin -- route_net
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

						when 13 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
							
						when others => command_incomplete;
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
										device		=> to_device_name (f (9)),
										terminal	=> to_terminal_name (f (10)),
										end_point	=> type_point (set (
												x => to_distance (f (12)),	 -- 35
												y => to_distance (f (13)))), -- 40
										
										log_threshold	=> log_threshold + 1
										);
									
								when 14 .. count_type'last =>
									command_too_long (single_cmd_status.cmd, fields - 1);
									
								when others =>
									command_incomplete;
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
											device		=> to_device_name (f (9)),
											terminal	=> to_terminal_name (f (10)),
											axis		=> to_axis (f (12)),
											notches		=> to_notches (f (13)), -- 5
											
											log_threshold	=> log_threshold + 1
											);
										
									when 14 .. count_type'last =>
										command_too_long (single_cmd_status.cmd, fields - 1);
										
									when others =>
										command_incomplete;
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
										device		=> to_device_name (f (9)),
										terminal	=> to_terminal_name (f (10)),
										direction	=> to_rotation (f (12)), -- 45 degree
										length		=> to_distance (f (13)), -- 50mm
										
										log_threshold	=> log_threshold + 1
										);

								when 14 .. count_type'last =>
									command_too_long (single_cmd_status.cmd, fields - 1);
									
								when others =>
									command_incomplete;
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
											device		=> to_device_name (f (9)),
											terminal	=> to_terminal_name (f (10)),
											direction	=> to_rotation (f (12)), -- 45 degree
											axis		=> to_axis (f (13)),
											notches		=> to_notches (f (14)), -- 5
											
											log_threshold	=> log_threshold + 1
											);

									when 15 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
										
									when others => 
										command_incomplete;
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
						
					when 16 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
						
					when others =>
						command_incomplete;
				end case;

			when POLYGON =>
				case fields is
					-- The polygon command is very long. The following example spreads across
					-- several lines:
					--  board led_driver route net RESET_N 1 polygon /
					--  line 0 0 100 0 /
					--  line 100 0 100 100 / 
					--  arc 50 100 100 100 0 100 ccw / 
					--  line 0 100 0 0
					when 6 .. count_type'last =>
						make_polygon;

					when others =>
						command_incomplete;
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
		case runmode is
			when MODE_MODULE =>

				log (text => "center on point", level => log_threshold + 1);
				center_on (canvas, c);

			when others =>
				skipped_in_this_runmode (log_threshold + 1);
				
		end case;
	end zoom_center;

	procedure set_scale (scale : in string) is  -- GUI related -- CS should be percent of scale_to_fit
		use glib;
		s : gdouble := gdouble'value (scale);
	begin
		case runmode is
			when MODE_MODULE =>

				log (text => "zoom level", level => log_threshold + 1);
				set_scale (canvas, s);

			when others =>
				skipped_in_this_runmode (log_threshold + 1);
				
		end case;
	end set_scale;
	
	-- Positions the cursor absolute or relative:
	procedure position_cursor is -- GUI related
		use et_geometry;
		
		coordinates : type_coordinates := to_coordinates (f (5));
		position : type_point := type_point (set (
				x => to_distance (f (6)),
				y => to_distance (f (7))));
	begin
		case runmode is
			when MODE_MODULE =>

				log (text => "place cursor" & to_string (coordinates) 
					& to_string (position), level => log_threshold + 1);
		
				canvas.move_cursor (coordinates, cursor_main, position);

			when others =>
				skipped_in_this_runmode (log_threshold + 1);
				
		end case;
	end position_cursor;

	procedure add_device is -- non-electric device !
		model : constant pac_package_model_file_name.bounded_string := to_file_name (f (5));
		prefix : constant pac_device_prefix.bounded_string := to_prefix (f (6));

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
			device_name		=> to_device_name (f (5)),
			log_threshold	=> log_threshold + 1);

	end delete_device;

	procedure rename_device is -- non-electric device !
		-- board led_driver rename device FD1 FD3
	begin
		rename_device (
			module_name			=> module,
			device_name_before	=> to_device_name (f (5)),
			device_name_after	=> to_device_name (f (6)),
			log_threshold		=> log_threshold + 1);

	end rename_device;

	-- Parses the single_cmd_status.cmd:
	procedure parse is begin
		log (text => "parsing command: " 
			& enclose_in_quotes (to_string (single_cmd_status.cmd)),
			level => log_threshold);

		-- Clear the status bar if we are in graphical mode:
		if runmode /= MODE_HEADLESS then
			status_clear;
		end if;
		
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
							
							when others => command_incomplete;
						end case;

					when NOUN_LAYER =>
						case fields is
							when 6 =>
								-- board tree_1 add layer 0.12 0.2
								add_layer;

							when 7 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
						end case;

					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_DELETE =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 5 => delete_device; -- board led_driver delete device FD1

							when 6 .. count_type'last => too_long;
							
							when others => command_incomplete;
						end case;

					when NOUN_LAYER =>
						case fields is
							when 5 =>
								-- board tree_1 delete layer 2
								delete_layer (
									module_name 	=> module,
									layer			=> to_signal_layer (f (5)),
									
									log_threshold	=> log_threshold + 1);

							when 6 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;

						end case;

					when NOUN_HOLE =>
						delete_hole_segment;

					when NOUN_OUTLINE =>
						delete_outline_segment;
						
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
								command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others =>
								command_incomplete;
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

							when 9 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
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

							when 9 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
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

							when 9 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
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

							when 9 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
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

							when 8 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
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

							when 8 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
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
							when others => command_incomplete;
						end case;
						
					when NOUN_SILKSCREEN -- like "board led_driver display silkscreen top [on/off]"
						| NOUN_ASSY | NOUN_KEEPOUT | NOUN_STOP | NOUN_STENCIL | NOUN_ORIGINS =>
						case fields is
							when 5 => display_non_conductor_layer (noun, f (5)); -- if status is omitted
							when 6 => display_non_conductor_layer (noun, f (5), f (6));
							when 7 .. count_type'last => too_long;
							when others => command_incomplete;
						end case;

					when NOUN_CONDUCTORS => -- like "board led_driver display conductors 2 [on/off]"
						case fields is
							when 5 => display_conductor_layer (f (5)); -- if status is omitted
							when 6 => display_conductor_layer (f (5), f (6));
							when 7 .. count_type'last => too_long;
							when others => command_incomplete;
						end case;

					when NOUN_OUTLINE => -- like "board led_driver display outline [on/off]"
						case fields is
							when 4 => display_outline; -- if status is omitted
							when 5 => display_outline (f (5));
							when 6 .. count_type'last => too_long;
							when others => command_incomplete;
						end case;
						
					when NOUN_RESTRICT => -- like "board led_driver display restrict route/via 2 [on/off]"
						case fields is
							when 6 => display_restrict_layer (f (5), f (6)); -- if status is omitted
							when 7 => display_restrict_layer (f (5), f (6), f (7));
							when 8 .. count_type'last => too_long;
							when others => command_incomplete;
						end case;

					when NOUN_VIAS => -- like "board led_driver display vias [on/off]"
						case fields is
							--when 5 => display_vias (f (5)); -- if status is omitted
							--when 6 => display_vias (f (5), f (6));
							--when 7 .. count_type'last => too_long;
							when 4 => display_vias; -- if status is omitted
							when 5 => display_vias (f (5));
							when 6 .. count_type'last => too_long;
							when others => command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_DRAW =>
				case noun is
					when NOUN_HOLE =>
						draw_hole;
						
					when NOUN_OUTLINE =>
						draw_outline;

					when NOUN_SILKSCREEN =>
						draw_silkscreen;

					when NOUN_ASSY =>
						draw_assy_doc;

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
							when others => command_incomplete;
						end case;
							
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_EXIT | VERB_QUIT => terminate_main;

			when VERB_FILL =>
				case noun is
					when NOUN_POLYGON =>
						fill_conductor_polygons (module_cursor, log_threshold + 1);
						set_status ("conductor polygons filled");
						
					when others => 
						invalid_noun (to_string (noun));
				end case;
				
			when VERB_FLIP =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 6 =>
								flip_device (
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									face			=> to_face  (f (6)),  -- top/bottom
									log_threshold	=> log_threshold + 1
									);

							when 7 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
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
								command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others =>
								command_incomplete;
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
								command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others =>
								command_incomplete;
						end case;
						
					when NOUN_DEVICE =>
						case fields is
							when 8 =>
								move_device (
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									coordinates		=> to_coordinates (f (6)),  -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (7)),
														y => to_distance (f (8)))),
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last =>
								command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others =>
								command_incomplete;
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
								command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others =>
								command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_PLACE =>
				case noun is
					when NOUN_VIA	=> place_via;
					when NOUN_TEXT	=> place_text;
					when others		=> invalid_noun (to_string (noun));
				end case;
				
			when VERB_POSITION => -- GUI related
				case noun is 
					when NOUN_CURSOR =>
						case fields is
							when 7 => position_cursor; -- position cursor absolute/relative 25 30
							when 8 .. count_type'last => too_long;
							when others => command_incomplete;
						end case;

					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_RENAME =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 6 => rename_device; -- board led_driver renames device FD1 FD3

							when 7 .. count_type'last => too_long;
							
							when others => command_incomplete;
						end case;

					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_ROUTE =>
				case noun is
					when NOUN_FREETRACK =>
						route_freetrack;

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
								command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others =>
								command_incomplete;
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
								command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others =>
								command_incomplete;
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
									device_name		=> to_device_name (f (5)), -- IC1
									coordinates		=> to_coordinates (f (6)),  -- relative/absolute
									rotation		=> to_rotation (f (7)),
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last =>
								command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others =>
								command_incomplete;
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

							when 7 .. count_type'last => command_too_long (single_cmd_status.cmd, fields - 1);
								
							when others => command_incomplete;
						end case;

					when NOUN_POLYGON =>
						set_polygon_properties; -- conductor layers related
						
					when NOUN_VIA =>
						set_via_properties;

					when others => invalid_noun (to_string (noun));
				end case;
				
	-- 			when VERB_SHOW => -- GUI related
	-- 				case noun is
	-- 					when NOUN_DEVICE =>
	-- 						case fields is
	-- 							when 5 => null; -- CS
	-- 							when 6 .. count_type'last => too_long;
	-- 							when others => command_incomplete;
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

							when others => command_incomplete;
						end case;

					when NOUN_LEVEL => -- zoom level 3
						case fields is
							when 4 => 
								set_scale (f (5));

							when 6 .. count_type'last => too_long;

							when others => command_incomplete;
						end case;
						
					when NOUN_CENTER => -- zoom center 10 10
						case fields is
							when 6 =>  -- zoom center 10 10
								zoom_center;

							when 7 =>  -- zoom center 10 10 0.5
								zoom_center;
								set_scale (f (7));

							when 8 .. count_type'last => too_long;

							when others => command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
		end case;

		-- Update GUI if we are in graphical mode:
		if runmode /= MODE_HEADLESS then
			canvas.update_mode_display;
		end if;
			
	end parse;
	
	procedure propose_arguments is
		use et_canvas_board_devices;
		use et_canvas_board_texts;
		use et_canvas_board_vias;
		
		device_name		: et_devices.type_device_name;
		
		procedure module_name_missing is begin
			set_status (incomplete & module_missing);
		end module_name_missing;

		procedure device_name_missing is begin
			set_status (incomplete & device_missing);
			-- No menu required and not reasonable.
			-- It might become very long if there were hundreds of devices.
		end device_name_missing;

		procedure device_not_found is begin
			set_status ("ERROR: Device " & to_string (device_name) & " not found !");
		end device_not_found;

		procedure net_name_missing is begin
			set_status (incomplete & net_missing);
		end net_name_missing;

		
	begin -- propose_arguments
		log_command_incomplete (fields, log_threshold);

		case verb is
			when VERB_PLACE =>
				case noun is
					when NOUN_TEXT =>
						show_text_properties;
						single_cmd_status.finalization_pending := true;

					when NOUN_VIA =>
						case fields is
							when 4 => -- place via
								show_via_properties;
								single_cmd_status.finalization_pending := true;

							when 5 => -- place via RESET_N
								
								-- Preset the net name so that it is visible
								-- in the via properties bar. This requires
								-- to set up an indexed net stored in via_place.net:
								set (
									net		=> via_place.net, 
									name	=> to_net_name (f (5)));

								-- Update the index of the net
								-- and the current active module:
								set_index (
									net		=> via_place.net,
									module	=> module_cursor);
								
								show_via_properties;
								single_cmd_status.finalization_pending := true;

							when others => null;
						end case;
						
					when others => null; -- CS
				end case;

			when others => null; -- CS
		end case;
		
	end propose_arguments;
	
begin -- board_cmd
	log (text => "given command: " 
		 & enclose_in_quotes (to_string (cmd_in)),
		 level => log_threshold);

	-- Make a copy of the given command. In case the given command is incomplete
	-- and we are in graphical mode (non-headless) then
	-- this procedure interactively proposes arguments and completes the command.
	single_cmd_status := (cmd => cmd_in, others => <>);

	-- single_cmd_status.cmd will now be processed and interactively completed


	

	module := to_module_name (f (2)); -- motor_driver (without extension *.mod)
	-- CS: Becomes obsolete once all board ops use the
	-- given module_cursor.

	-- read the verb from field 3
	verb := to_verb (f (3));
	
	-- There are some very short commands which do not require a verb.
	-- For such commands we do not read the noun.
	case verb is
		when VERB_EXIT | VERB_QUIT => null; -- no noun
		when others => noun := to_noun (f (4)); -- read noun from field 4
	end case;

	-- Clean up:
	-- Some toolbars or property bars must be removed:
	et_canvas_board_texts.remove_text_properties; -- after placing text
	
	-- parse the command:
	parse;
	
	-- In case parse throws an exception, then the follwing statements 
	-- will be skipped.
	
	-- In graphical mode and cmd_entry_mode SINGLE_CMD the flag
	-- single_cmd_status.complete can change to false. In that case
	-- the interactive completiton starts here. 
	if not single_cmd_status.complete then
		propose_arguments;
	end if;

	-- After every command (regardless if it is complete or not)
	-- set the focus to the canvas:
	-- CS: remove ?
	if runmode /= MODE_HEADLESS then
		canvas.grab_focus;
	end if;


	exception when event: others =>

			evaluate_exception (
				name	=> exception_name (event),
				message	=> exception_message (event));

			raise;
	
end board_cmd;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        EXECUTE BOARD COMMNAD                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

with et_module_instance;			use et_module_instance;
with et_pcb_sides;

with et_schematic_coordinates;
with et_board_coordinates;

with et_board_text;
with et_board_layer_category;		use et_board_layer_category;
with et_board_ops.conductors;
with et_board_ops.vias;
with et_board_ops.frame;
with et_sheets;
with et_drills;
with et_modes.board;
with et_canvas_board_devices;
with et_canvas_board_texts;
with et_canvas_board_vias;
with et_design_rules_board;			use et_design_rules_board;
with et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_thermal_relief;				use et_thermal_relief;
with et_conductor_text;				use et_conductor_text;
with et_route_restrict.boards;		use et_route_restrict.boards;
with et_via_restrict.boards;		use et_via_restrict.boards;
with et_ratsnest;					use et_ratsnest;
with et_pcb_contour;
with et_pcb_placeholders;
with et_board_ops;
with et_board_ops.devices;
with et_board_ops.silkscreen;
with et_board_ops.assy_doc;
with et_board_ops.stopmask;
with et_board_ops.stencil;
with et_board_ops.keepout;
with et_board_ops.route_restrict;
with et_board_ops.via_restrict;
with et_board_ops.board_contour;
with et_board_ops.ratsnest;
with et_board_ops.text;
with et_board_ops.grid;

with et_canvas.cmd;

with et_drawing_frame;

with et_terminals;
with et_package_names;
with et_mirroring;
with et_device_prefix;
with et_vias;
with et_pcb;
with et_pcb_stack;
with et_keywords;					use et_keywords;

with et_canvas_board_preliminary_object;

-- to do:


separate (et_scripting)



procedure execute_board_command (
	module_cursor	: in pac_generic_modules.cursor;
	cmd				: in out type_single_cmd;
	log_threshold	: in type_log_level)
is
	use et_board_ops;
	use et_board_ops.conductors;
	use et_board_ops.vias;
	use et_board_text;
	use et_drills;
	use et_vias;
	use et_pcb;

	use et_pcb_sides;
	use et_board_coordinates;
	use pac_geometry_2;
	use pac_contours;

	use et_pcb_stack;
	use et_canvas_board_2;
	use et_canvas_board_2.pac_canvas;
	use et_display.board;
	use et_modes.board;

	use et_device_name;
	

	package pac_canvas_cmd is new et_canvas_board_2.pac_canvas.cmd;
	use pac_canvas_cmd;


	-- This function is a shortcut to get a single field
	-- from the given command:
	function get_field (place : in type_field_count) 
		return string 
	is begin
		return get_field (cmd, place);
	end;

	
	-- This procedure sets the verb and the noun:
	procedure set_verb_and_noun is begin
		-- Set the verb.
		-- Read it from field 3:
		verb := to_verb (get_field (3));

		
		-- There are some very short commands which do not require a noun.
		-- For such commands we do not read the noun.
		case verb is
			when VERB_EXIT | VERB_QUIT => null; -- no noun
			
			-- Set the noun. Read it from field 4:		
			when others => noun := to_noun (get_field (4));
		end case;
	end set_verb_and_noun;

	

	-- Updates the verb-noun display depending on the 
	-- origin of the command and the runmode:
	procedure update_verb_noun_display is begin
		case get_origin (cmd) is
			when ORIGIN_CONSOLE => update_mode_display;

			when ORIGIN_SCRIPT =>
				-- put_line ("script");
			
				if runmode = MODE_MODULE then
					-- put_line ("module");
					-- log (text => "update verb-noun-display", level => log_threshold + 1);
					update_mode_display;
				end if;

		end case;
	end update_verb_noun_display;

	
	
	module	: pac_module_name.bounded_string; -- motor_driver (without extension *.mod)


	-- Contains the number of fields given by the caller of this procedure:
	cmd_field_count : constant type_field_count := get_field_count (cmd);


	-- This procedure is a shortcut. 
	-- Call it in case the given command is too long:
	procedure too_long is begin
		command_too_long (get_fields (cmd), cmd_field_count - 1);
	end;


	-- This procedure is a shortcut. 
	-- Call it in case the given command is incomplete:
	procedure command_incomplete is begin
		command_incomplete (cmd);
	end;

	
	
	
	-- This procedure parses a zoom related command.
	-- If the runmode is non-graphical (like headless) then
	-- nothing will be done here:
	procedure zoom_all is begin
		-- log (text => "zoom all ...", level => log_threshold + 1);

		-- Zoom commands can only be executed in a graphical runmode:
		case runmode is
			when MODE_MODULE =>

				case noun is
					when NOUN_ALL => -- zoom all
						
						case cmd_field_count is
							when 4 => 
								log (text => "zoom all", level => log_threshold + 1);
								zoom_to_fit_all;

							when 5 .. type_field_count'last => too_long;

							when others => command_incomplete;
						end case;

					when others => 
						null;

				end case;

				
			when others =>
					skipped_in_this_runmode (log_threshold + 1);
					
		end case;				
	end zoom_all;


	

	-- This procedure parses the command to set the grid.
	-- It sets the grid of the canvas and assigns it in
	-- the database accordingly:
	procedure set_grid is 
		use et_board_ops.grid;
	begin
		-- Set the grid on the canvas:
		parse_canvas_command (cmd, VERB_SET, NOUN_GRID);

		-- The global variable "grid" has now been set
		-- as requested by the operator.
		
		-- Assign the grid in the database:
		set_grid (
			module_name 	=> module,
			grid			=> pac_canvas.grid,
			log_threshold	=> log_threshold + 1);

	end set_grid;

	
	

	-- This procedure parses the command to set the scale.
	-- CS: It is currently not complete.
	procedure set_scale is begin
		parse_canvas_command (cmd, VERB_SET, NOUN_SCALE);

		-- The global scale variable "M" has now been set
		-- as requested by the operator.
		
		-- CS: scale_objects (see demo program)

		-- CS: Assign the scale in the database.
	end set_scale;
	
	


	-- This procedure parses a command to display
	-- the board outline.
	-- Example command: board led_driver display outline [on/off]
	procedure display_outline is
		
		-- Enables/disables the outline layer. If status is empty,
		-- the layer will be enabled.
		procedure display ( -- GUI related
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
		end display;

		
	begin
		case cmd_field_count is
			when 4 => display; -- if status is omitted
			when 5 => display (get_field (5));
			when 6 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
	end display_outline;

	
	


	-- This procedure parses a command to display
	-- the ratsnest (unrouted, airwires).
	-- Example command: board led_driver display ratsnest [on/off]
	procedure display_ratsnest is

		-- Enables/disables the ratsnest layer. If status is empty,
		-- the layer will be enabled.
		procedure display ( -- GUI related
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

			log (text => "display ratsnest layer" & space & to_string (ls),
					level => log_threshold + 1);

			layers.ratsnest := ls;
			
			-- CS exception handler if status is invalid
		end display;
		
		
	begin
		case cmd_field_count is
			when 4 => display; -- if status is omitted
			when 5 => display (get_field (5));
			when 6 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
	end display_ratsnest;



	
	

	-- This procedure parses a command to display
	-- a non-conductor layer (silkscreen, stopmask, assembly doc, ...).
	-- Example command: board led_driver display silkscreen top [on/off]
	procedure display_non_conductor_layer is

		-- Enables/disables a certain non-conductor layer. If status is empty,
		-- the layer will be enabled.
		procedure display ( -- GUI related
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

			-- CS: use commands like enable_silkscreen (top) ? (see et_display.board)

			case fc is
				when TOP =>
					case layer is
						when NOUN_SILKSCREEN 	=> layers.silkscreen.top		:= ls;
						when NOUN_ASSY			=> layers.assy_doc.top			:= ls;
						when NOUN_KEEPOUT		=> layers.keepout.top			:= ls;
						when NOUN_STENCIL		=> layers.stencil.top			:= ls;
						when NOUN_STOPMASK		=> layers.stop_mask.top			:= ls;
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
						when NOUN_STOPMASK		=> layers.stop_mask.bottom		:= ls;
						when NOUN_ORIGINS		=> layers.device_origins.bottom	:= ls;
						
						when others => 
							log (importance => ERROR, text => "invalid layer !", console => true);
					end case;
			end case;
			
			-- CS exception handler if status is invalid
		end display;

		
	begin
		case cmd_field_count is
			when 5 => display (noun, get_field (5)); -- if status is omitted
			when 6 => display (noun, get_field (5), get_field (6));
			when 7 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
	end display_non_conductor_layer;
	

	


	-- This procedure parses a command to display
	-- a conductor layer.
	-- Example command: board led_driver display conductors 2 [on/off]
	procedure display_conductor_layer is
	
		-- Enables/disables a certain conductor layer. 
		-- If status is empty, the layer will be enabled.
		procedure display ( -- GUI related
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
		end display;

		
	begin
		case cmd_field_count is
			when 5 => display (get_field (5)); -- if status is omitted
			when 6 => display (get_field (5), get_field (6));
			when 7 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
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


	
	procedure draw_board_outline is
		use et_board_ops.board_contour;
		use et_pcb_contour;
		
		-- Extract from the given command the 
		-- arguments (everything after "outline"):
		-- Example command: 
		-- "board demo draw outline line 0 0 line 50 0 line 50 50 line 0 50"
		arguments : constant type_fields_of_line := 
			remove_field (get_fields (cmd), 1, 4);

		-- Build a basic contour from the arguments:
		c : constant type_contour := type_contour (to_contour (arguments));
	begin
		-- Convert the contour to a pcb outer edge type
		-- and assign it to the module:
		set_outline (module_cursor, (c with null record), log_threshold + 1);
	end draw_board_outline;


	
	
	procedure draw_hole is
		use et_board_ops.board_contour;
		
		-- Extract from the given command the 
		-- arguments (everything after "hole"):
		-- example command: board demo draw hole line 2 9 line 2 1 line 8 9
		arguments : constant type_fields_of_line := 
			remove_field (get_fields (cmd), 1, 4);

		-- Build a basic contour from the arguments:
		c : constant type_contour := type_contour (to_contour (arguments));
	begin
		-- Convert the contour to an inner pcb edge type and add it to
		-- the already existing holes:
		set_hole (module_cursor, (c with null record), log_threshold + 1);
	end draw_hole;




	

	-- This procedure parses a command that draws
	-- a keepout zone:
	procedure draw_keepout is
		use et_board_ops.keepout;

		-- Extract from the given command the zone 
		-- arguments (everything after "keepout"):
		-- example command: 
		-- board demo draw keepout top zone line 0 0 line 10 0 line 10 10 line 0 10
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			face : type_face;
		begin
			face := to_face (get_field (5));
			
			add_zone (
				module_cursor	=> module_cursor,
				zone			=> (c with null record),
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end build_zone;

		
	begin
		-- Convert the contour to a keepout zone
		-- and assign it to the module:

		if get_field (6) = keyword_zone then
			build_zone;
		else
			null;
			-- CS error. only zone allowed here
		end if;
	end draw_keepout;


	
	
	procedure delete_outline_segment is 
		use et_board_ops.board_contour;
		catch_zone : type_catch_zone;
	begin
		case cmd_field_count is
			when 7 =>
				-- delete a segment of the outer board contour:

				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (5), get_field (6)),
					radius	=> to_zone_radius (get_field (7)));
					
				delete_outer_segment (
					module_cursor 	=> module_cursor,
					catch_zone		=> catch_zone,
					log_threshold	=> log_threshold + 1);

			when 8 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_outline_segment;


	
	
	procedure delete_hole_segment is 
		use et_board_ops.board_contour;
		catch_zone : type_catch_zone;
	begin
		case cmd_field_count is
			when 7 =>
				-- delete a segment of a hole

				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (5), get_field (6)),
					radius	=> to_zone_radius (get_field (7)));

				delete_hole_segment (
					module_cursor 	=> module_cursor,
					catch_zone		=> catch_zone,					
					log_threshold	=> log_threshold + 1);

			when 8 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_hole_segment;

	


	-- This procedure parses a command that draws
	-- a line, arc, circle or a zone in silkscreen:
	procedure draw_silkscreen is
		use et_board_ops.silkscreen;

		-- Extract from the given command the zone arguments (everything after "zone"):
		-- example command: board demo draw silkscreen top zone line 0 0 line 50 0 line 50 50 line 0 50
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			face : type_face;
		begin
			face := to_face (get_field (5));
			
			add_zone (
				module_cursor	=> module_cursor,
				zone			=> (c with null record),
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end build_zone;

		
		shape : type_shape;

		
		-- Draws a line, arc or circle:
		procedure draw_shape is 
			arc_tmp		: type_arc;
			circle_tmp	: type_circle;
			line_tmp	: type_line;
			width_tmp	: type_distance_positive;
		begin
			case shape is
				when LINE =>
					case cmd_field_count is
						when 11 =>
							width_tmp := to_distance (get_field (7));
							
							line_tmp := type_line (to_line (
								A => to_vector_model (get_field (8), get_field (9)),
								B => to_vector_model (get_field (10), get_field (11))));
							
							add_line (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								line			=> (line_tmp with width_tmp),
								log_threshold	=> log_threshold + 1
								);

						when 12 .. type_field_count'last =>
							too_long;
							
						when others =>
							command_incomplete;
					end case;

					
				when ARC =>
					case cmd_field_count is
						when 14 =>
							width_tmp := to_distance (get_field (7));
							
							arc_tmp := type_arc (to_arc (
								center		=> to_vector_model (get_field (8), get_field (9)),
								A			=> to_vector_model (get_field (10), get_field (11)),
								B			=> to_vector_model (get_field (12), get_field (13)),
								direction	=> to_direction (get_field (14))));
							
							add_arc (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								arc				=> (arc_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);

						when 15 .. type_field_count'last =>
							too_long;
							
						when others =>
							command_incomplete;
					end case;

					
				when CIRCLE =>
					case cmd_field_count is						
						when 10 =>
							width_tmp := to_distance (get_field (7));
							
							circle_tmp := type_circle (to_circle (
								center		=> to_vector_model (get_field (8), get_field (9)),
								radius		=> to_radius (get_field (10))));
													  
							add_circle (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								circle			=> (circle_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);
							
						when 11 .. type_field_count'last =>
							too_long;
							
						when others =>
							command_incomplete;
					end case;
							
				when others => null;
			end case;
		end draw_shape;
		

	begin
		if get_field (6) = keyword_zone then
			build_zone;
		else
			shape := to_shape (get_field (6));
			draw_shape;
		end if;		
	end draw_silkscreen;


	

	-- This procedure parses a command that draws
	-- a line, arc, circle or a zone in assembly documentation:
	procedure draw_assy_doc is
		use et_board_ops.assy_doc;
		
		-- Extract from the given command the zone arguments (everything after "zone"):
		-- example command: board demo draw assy top zone line 0 0 line 50 0 line 50 50 line 0 50
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			face : type_face;
		begin
			face := to_face (get_field (5));
			
			add_zone (
				module_cursor	=> module_cursor,
				zone			=> (c with null record),
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end build_zone;

		
		shape : type_shape;

		
		-- Draws a line, arc or circle:
		procedure draw_shape is 
			arc_tmp		: type_arc;
			circle_tmp	: type_circle;
			line_tmp	: type_line;
			width_tmp	: type_distance_positive;
		begin
			case shape is
				when LINE =>
					case cmd_field_count is
						when 11 =>
							width_tmp := to_distance (get_field (7));

							line_tmp := type_line (to_line (
								A => to_vector_model (get_field (8), get_field (9)),
								B => to_vector_model (get_field (10), get_field (11))));
								
							add_line (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								line			=> (line_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);

						when 12 .. type_field_count'last =>
							too_long;
							
						when others =>
							command_incomplete;
					end case;

					
				when ARC =>
					case cmd_field_count is
						when 14 =>
							width_tmp := to_distance (get_field (7));
							
							arc_tmp := type_arc (to_arc (
								center	=> to_vector_model (get_field (8), get_field (9)),
								A		=> to_vector_model (get_field (10), get_field (11)),
								B		=> to_vector_model (get_field (12), get_field (13)),
								direction	=> to_direction (get_field (14))));
															
							add_arc (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								arc				=> (arc_tmp with width_tmp),
								log_threshold	=> log_threshold + 1
								);

						when 15 .. type_field_count'last =>
							too_long;
							
						when others =>
							command_incomplete;
					end case;

					
				when CIRCLE =>
					case cmd_field_count is
						when 10 =>
							width_tmp := to_distance (get_field (7));
							
							circle_tmp := type_circle (to_circle (
								center		=> to_vector_model (get_field (8), get_field (9)),
								radius		=> to_radius (get_field (10))));
										
							add_circle (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								circle			=> (circle_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);
							
						when 11 .. type_field_count'last =>
							too_long;
							
						when others =>
							command_incomplete;
					end case;
							
				when others => null;
			end case;
		end draw_shape;
		

	begin
		if get_field (6) = keyword_zone then
			build_zone;
		else
			shape := to_shape (get_field (6));
			draw_shape;
		end if;		
	end draw_assy_doc;

	


	-- This procedure parses a command that draws
	-- a line, arc, circle or a zone in route restrict:
	procedure draw_route_restrict is
		use et_board_ops.route_restrict;

		-- Extract from the given command the zone arguments (everything after "zone"):
		-- example command: board demo draw route_restrict [1] zone line 0 0 line 50 0 line 50 50 line 0 50
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			l : type_signal_layers.set;
		begin
			l := to_layers (get_field (5));
			
			draw_zone (
				module_cursor	=> module_cursor,
				zone			=> (c with l),
				log_threshold	=> log_threshold + 1);

		end build_zone;
		
		
		shape : type_shape;

		line_tmp	: type_line;
		arc_tmp		: type_arc;
		circle_tmp	: type_circle;
		layers_tmp	: type_signal_layers.set;
		
	begin
		-- put_line ("draw_route_restrict");

		if get_field (6) = keyword_zone then
			build_zone;
		else
			shape := to_shape (get_field (6));
			-- CS apply assigment to shape to all similar procedures !			

			
			case shape is
				when LINE =>
					case cmd_field_count is
						when 10 =>
							-- board led_driver draw route_restrict [1,3,5-9] line 10 10 60 10
							-- CS board led_driver draw route_restrict 3 line 10 10 60 10
							
							-- CS test whether field 5 is a single layer id. If yes then
							-- call function et_pcb_stack.to_signal_layer to get the id-type.
							-- Then validate signal layer.
							-- Then add the single signal layer to a set.
							-- Do so with all objects in route and via restrict.

							line_tmp := type_line (to_line (
								A => to_vector_model (get_field (7), get_field  (8)),
								B => to_vector_model (get_field (9), get_field (10))));

							layers_tmp := to_layers (get_field (5)); -- [1,3,5-9]
							
							draw_route_restrict_line (
								module_name 	=> module,
								line			=> (line_tmp with layers_tmp),
								log_threshold	=> log_threshold + 1);

						when 11 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;

					
				when ARC =>
					case cmd_field_count is
						when 13 =>							
							-- board led_driver draw route_restrict [1,3,5-9] arc 50 50 0 50 100 0 cw

							layers_tmp := to_layers (get_field (5)); -- [1,3,5-9]
							
							arc_tmp := type_arc (to_arc (
								center		=> to_vector_model (get_field  (7), get_field  (8)),
								A			=> to_vector_model (get_field  (9), get_field (10)),
								B			=> to_vector_model (get_field (11), get_field (12)),
								direction	=> to_direction (get_field (13))));
															
							draw_route_restrict_arc (
								module_name 	=> module,
								arc				=> (arc_tmp with layers_tmp),
								log_threshold	=> log_threshold + 1);

						when 14 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;

					
				when CIRCLE =>
					case cmd_field_count is
						when 9 =>
							-- board led_driver draw route_restrict [1,3,5-9] circle 20 50 40
							-- if is_number (get_field (7)) then -- 20

							layers_tmp := to_layers (get_field (5)); -- [1,3,5-9]

							circle_tmp := type_circle (to_circle (
								center	=> to_vector_model (get_field (7), get_field (8)),
								radius	=> to_radius (get_field (9)))); -- 40
																	 
								-- Circle is not filled.
								draw_route_restrict_circle (
									module_name 	=> module,
									circle			=> (circle_tmp with layers_tmp),
									log_threshold	=> log_threshold + 1);
								
							-- else
								-- expect_value_center_x (7);
							-- end if;

						--when 10 =>
							---- Circle is filled.
							---- board led_driver draw route_restrict [1,3,5-9] circle filled 20 50 40
							--if get_field (7) = keyword_filled then

								---- Circle is filled.
								--draw_route_restrict_circle (
									--module_name 	=> module,
									--circle			=> 
												--(
												--layers		=> to_layers (get_field (5)), -- [1,3,5-9]
												--filled		=> YES,
												--center	=> to_vector_model (get_field (8), get_field (9))),
												--radius	=> to_radius (get_field (10)) -- 40
												--),
												
									--log_threshold	=> log_threshold + 1);
							--else
								--expect_keyword_filled (7);
							--end if;

						when 10 .. type_field_count'last => too_long;
						
						when others => command_incomplete;
					end case;
							
				when others => null;
			end case;
		end if;
			
	end draw_route_restrict;




	-- This procedure parses a command that draws
	-- a line, arc, circle or a zone in via restrict:
	procedure draw_via_restrict is
		use et_board_ops.via_restrict;

		-- Extract from the given command the zone arguments (everything after "zone"):
		-- example command: board demo draw via_restrict [1] zone line 0 0 line 50 0 line 50 50 line 0 50
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			l : type_signal_layers.set;
		begin
			l := to_layers (get_field (5));
			
			draw_zone (
				module_cursor	=> module_cursor,
				zone			=> (c with l),
				log_threshold	=> log_threshold + 1);

		end build_zone;

		
	begin
		if get_field (6) = keyword_zone then
			build_zone;
		else
			null;
			-- CS error. only zone allowed here
		end if;
		
	end draw_via_restrict;



	
	
	procedure draw_stop_mask is
		use et_board_ops.stopmask;

		-- Extract from the given command the zone arguments (everything after "zone"):
		-- example command: board demo draw stop top zone line 0 0 line 50 0 line 50 50 line 0 50
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			face : type_face;
		begin
			face := to_face (get_field (5));
			
			add_zone (
				module_cursor	=> module_cursor,
				zone			=> (c with null record),
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end build_zone;


		
		shape : type_shape;

		-- Draws a line, arc or circle:
		procedure draw_shape is 
			line_tmp	: type_line;
			arc_tmp 	: type_arc;
			circle_tmp	: type_circle;
			width_tmp	: type_distance_positive;
		begin
			case shape is
				when LINE =>
					case cmd_field_count is
						when 11 =>
							width_tmp := to_distance (get_field (7));

							line_tmp := type_line (to_line (
								A => to_vector_model (get_field (8), get_field (9)),
								B => to_vector_model (get_field (10), get_field (11))));
															   
							add_line (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								line			=> (line_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);

						when 12 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;

					
				when ARC =>
					case cmd_field_count is
						when 14 =>
							width_tmp := to_distance (get_field (7));
							
							arc_tmp := type_arc (to_arc (
								center		=> to_vector_model (get_field (8), get_field (9)),
								A			=> to_vector_model (get_field (10), get_field (11)),
								B			=> to_vector_model (get_field (12), get_field (13)),
								direction	=> to_direction (get_field (14))));
															
							add_arc (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								arc				=> (arc_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);

						when 15 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;

					
				when CIRCLE =>
					case cmd_field_count is
						when 10 =>
							width_tmp := to_distance (get_field (7));
							
							circle_tmp := type_circle (to_circle (
								center			=> to_vector_model (get_field (8), get_field (9)),
								radius			=> to_radius (get_field (10))));
																	 
							add_circle (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								circle			=> (circle_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);
							
						when 11 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;
			end case;
		end draw_shape;

		
	begin
		if get_field (6) = keyword_zone then
			build_zone;
		else
			shape := to_shape (get_field (6));
			draw_shape;
		end if;		
	end draw_stop_mask;


	
	
	procedure draw_stencil is
		use et_board_ops.stencil;

		-- Extract from the given command the zone arguments (everything after "stencil"):
		-- example command: 
		-- board demo draw stencil top zone line 0 0 line 10 0 line 10 10 line 0 10
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			face : type_face;
		begin
			face := to_face (get_field (5));
			
			draw_zone (
				module_cursor	=> module_cursor,
				zone			=> (c with null record),
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end build_zone;


		
		shape : type_shape;

		-- Draws a line, arc or circle:
		procedure draw_shape is
			line_tmp	: type_line;
			arc_tmp 	: type_arc;
			circle_tmp	: type_circle;
			width_tmp	: type_distance_positive;
		begin
			case shape is
				when LINE =>
					case cmd_field_count is
						when 11 =>
							width_tmp := to_distance (get_field (7));

							line_tmp := type_line (to_line (
								A => to_vector_model (get_field (8), get_field (9)),
								B => to_vector_model (get_field (10), get_field (11))));
							
							add_line (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								line			=> (line_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);

						when 12 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;

					
				when ARC =>
					case cmd_field_count is
						when 14 =>
							width_tmp := to_distance (get_field (7));
							
							arc_tmp := type_arc (to_arc (
								center	=> to_vector_model (get_field (8), get_field (9)),
								A		=> to_vector_model (get_field (10), get_field (11)),
								B		=> to_vector_model (get_field (12), get_field (13)),
								direction	=> to_direction (get_field (14))));
															
							add_arc (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								arc				=> (arc_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);

						when 15 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;

					
				when CIRCLE =>
					case cmd_field_count is
						when 10 =>
							width_tmp := to_distance (get_field (7));
							
							circle_tmp := type_circle (to_circle (
								center		=> to_vector_model (get_field (8), get_field (9)),
								radius		=> to_radius (get_field (10))));
																	 
							add_circle (
								module_name 	=> module,
								face			=> to_face (get_field (5)),
								circle			=> (circle_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);

						when 11 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;
							
				-- when others => null;
			end case;
			
		end draw_shape;

		
		
	begin
		if get_field (6) = keyword_zone then
			build_zone;
		else
			shape := to_shape (get_field (6));
			draw_shape;
		end if;
	end draw_stencil;


	
	

	-- This procedure parses a command to place text
	-- and dispatches to subprogram according to the layer category:
	procedure place_text is
		use pac_text_board;
		use et_board_ops.text;
		use et_mirroring;
		
		text			: type_text_fab_with_content;
		pos_xy			: type_vector_model;
		rotation		: type_rotation_model;
		content			: pac_text_content.bounded_string;
		layer_category	: type_layer_category;
		
		face			: type_face;

		procedure place_in_assy_doc is
			use et_board_ops.assy_doc;
		begin
			add_text (
				module_cursor 	=> module_cursor,
				face			=> face,
				text			=> text,
				log_threshold	=> log_threshold + 1);
		end place_in_assy_doc;


		procedure place_in_silkscreen is
			use et_board_ops.silkscreen;
		begin
			add_text (
				module_cursor 	=> module_cursor,
				face			=> face,
				text			=> text,
				log_threshold	=> log_threshold + 1);
		end place_in_silkscreen;


		procedure place_in_stopmask is
			use et_board_ops.stopmask;
		begin
			add_text (
				module_cursor 	=> module_cursor,
				face			=> face,
				text			=> text,
				log_threshold	=> log_threshold + 1);
		end place_in_stopmask;


		procedure place_in_conductor_layer is
			use et_board_ops.conductors;
			signal_layer	: type_signal_layer;
		begin
			signal_layer := to_signal_layer (get_field (6));  -- 5 
							
			-- This procedure automatically cares for mirroring:
			add_text (
				module_cursor 	=> module_cursor,
				signal_layer	=> signal_layer,
				text			=> text,
				log_threshold	=> log_threshold + 1);
		end place_in_conductor_layer;
		
		
	begin
		-- board demo place text silkscreen top 0.15 1 140 100 0 "SILKSCREEN"
		-- board demo place text conductor  5   0.15 1 140 100 0 "L1"

		-- CS: argument for alignment

		
		case cmd_field_count is
			when 12 =>
				layer_category := to_layer_category (get_field (5));
				text.line_width := to_distance (get_field (7)); -- 0.15
				text.size := to_distance (get_field (8)); -- 1
				
				pos_xy := to_vector_model (get_field (9), get_field (10));

				rotation := to_rotation (get_field (11)); -- 0
				text.position := type_position (to_position (pos_xy, rotation));
				
				text.content := to_content (get_field (12));
				-- CS check length

				
				if characters_valid (content) then

					case layer_category is
						when LAYER_CAT_ASSY =>
							face := to_face (get_field (6)); -- top/bottom
							place_in_assy_doc;

							
						when LAYER_CAT_SILKSCREEN =>
							face := to_face (get_field (6)); -- top/bottom
							place_in_silkscreen;

							
						when LAYER_CAT_STOPMASK =>						
							face := to_face (get_field (6)); -- top/bottom
							place_in_stopmask;

							
						
						when LAYER_CAT_CONDUCTOR =>
							place_in_conductor_layer;
							

						when others => null; -- CS message invalid layer category ?
					end case;

				else
					raise syntax_error_1 with
						"ERROR: Invalid character in text !";
					-- CS show invalid character and its position
				end if;
					
			when 13 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end place_text;





	
	-- This procedure parses a command to place text placeholder
	-- and dispatches to subprogram according to the layer category:	
	procedure place_text_placeholder is
		use pac_text_board;
		use et_pcb_placeholders;
		use et_mirroring;
		
		pos_xy			: type_vector_model;
		rotation		: type_rotation_model;
		size			: type_distance;
		linewidth		: type_distance;
		layer_category	: type_layer_category;
		face			: type_face;

		
		procedure place_in_assy_doc is
			use et_board_ops.assy_doc;
			ph : type_text_placeholder; -- non conductor layers
		begin
			ph.meaning := to_meaning (get_field (12));
			ph.position := type_position (to_position (pos_xy, rotation));
			ph.line_width := linewidth;
			ph.size := size;
			
			add_placeholder (
				module_cursor 	=> module_cursor,
				placeholder		=> ph,
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end place_in_assy_doc;



		procedure place_in_silkscreen is
			use et_board_ops.silkscreen;
			ph : type_text_placeholder; -- non conductor layers
		begin
			ph.meaning := to_meaning (get_field (12));
			ph.position := type_position (to_position (pos_xy, rotation));
			ph.line_width := linewidth;
			ph.size := size;

			add_placeholder (
				module_cursor 	=> module_cursor,
				placeholder		=> ph,
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end place_in_silkscreen;



		procedure place_in_stopmask is
			use et_board_ops.stopmask;
			ph : type_text_placeholder; -- non conductor layers
		begin
			ph.meaning := to_meaning (get_field (12));
			ph.position := type_position (to_position (pos_xy, rotation));
			ph.line_width := linewidth;
			ph.size := size;

			add_placeholder (
				module_cursor 	=> module_cursor,
				placeholder		=> ph,
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end place_in_stopmask;

		
		
		procedure place_in_conductor_layer is
			ph : type_text_placeholder_conductors; -- conductor layers
		begin
			ph.layer := to_signal_layer (get_field (6));  -- 5 
			ph.meaning := to_meaning (get_field (12));
			ph.position := type_position (to_position (pos_xy, rotation));
			ph.line_width := linewidth;
			ph.size := size;
			
			-- This procedure automatically cares for mirroring:
			add_placeholder (
				module_cursor 	=> module_cursor,
				placeholder		=> ph,
				log_threshold	=> log_threshold + 1);

		end place_in_conductor_layer;


		
	begin
		-- board demo place placeholder silkscreen top 0.15 1 140 100 0 module
		-- board demo place placeholder conductor  5   0.15 1 140 100 0 module

		-- CS: argument for alignment

		case cmd_field_count is
			when 12 =>
				layer_category := to_layer_category (get_field (5));

				-- Get the linewidth of the placeholder:
				linewidth := to_distance (get_field (7)); -- 0.15
				validate_text_line_width (linewidth);

				-- Get the size of the placeholder:
				size := to_distance (get_field (8)); -- 1
				validate_text_size (size);
				
				-- Get the position of the placeholder:
				pos_xy := to_vector_model (get_field (9), get_field (10));
				rotation := to_rotation (get_field (11)); -- 0
				
				
				case layer_category is
					when LAYER_CAT_ASSY =>

						face := to_face (get_field (6)); -- top/bottom
						place_in_assy_doc;


					when LAYER_CAT_SILKSCREEN =>

						face := to_face (get_field (6)); -- top/bottom
						place_in_silkscreen;


					when LAYER_CAT_STOPMASK =>

						face := to_face (get_field (6)); -- top/bottom
						place_in_stopmask;

					
					when LAYER_CAT_CONDUCTOR =>
						place_in_conductor_layer;


					when others => null; -- CS message invalid layer category ?
				end case;
					
			when 13 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end place_text_placeholder;


	
	
	
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

		
		procedure deactivate_drill (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.drill.active := false;
		end deactivate_drill;

		
		procedure activate_drill (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.drill.active := true;
			module.board.user_settings.vias.drill.size := to_distance (get_field (6));
		end activate_drill;

		
		procedure deactivate_inner_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.restring_inner.active := false;
		end deactivate_inner_restring;

		
		procedure activate_inner_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.restring_inner.active := true;
			module.board.user_settings.vias.restring_inner.width := to_distance (get_field (7));
		end activate_inner_restring;

		
		procedure deactivate_outer_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.restring_outer.active := false;
		end deactivate_outer_restring;

		
		procedure activate_outer_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.restring_outer.active := true;
			module.board.user_settings.vias.restring_outer.width := to_distance (get_field (7));
		end activate_outer_restring;

		
	begin -- set_via_properties
		case cmd_field_count is
			when 6 => 
				-- board demo set via drill 0.3/dru
				if get_field (5) = kw_drill then
					if get_field (6) = kw_dru then
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
				if get_field (5) = kw_restring then

					-- board demo set via restring inner 0.2
					-- board demo set via restring inner dru
					if get_field (6) = kw_inner then
						if get_field (7) = kw_dru then
							update_element (generic_modules, module_cursor, deactivate_inner_restring'access);
						else
							update_element (generic_modules, module_cursor, activate_inner_restring'access);
							
							-- CS validate against dru settings	
						end if;

					-- board demo set via restring outer 0.2
					-- board demo set via restring outer dru
					elsif get_field (6) = kw_outer then
						if get_field (7) = kw_dru then
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
				
			when 8 .. type_field_count'last => too_long;

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
			-- CS check net name: characters, length, existence of net
			net_name := to_net_name (get_field (5));
		end set_net_name;

		
		procedure set_position is begin
			drill.position := to_vector_model (get_field (6), get_field (7));

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
					
			et_board_ops.vias.place_via (module_cursor, net_name, via, log_threshold + 1);
		end through;

		
		procedure blind_top is
			via : type_via (BLIND_DRILLED_FROM_TOP);
		begin
			via := (drill with
				category		=> BLIND_DRILLED_FROM_TOP,
				restring_inner	=> restring_inner,
				restring_top	=> restring_top,
				lower			=> lower_layer);
					
			et_board_ops.vias.place_via (module_cursor, net_name, via, log_threshold + 1);
		end blind_top;

		
		procedure blind_bottom is
			via : type_via (BLIND_DRILLED_FROM_BOTTOM);
		begin
			via := (drill with
				category		=> BLIND_DRILLED_FROM_BOTTOM,
				restring_inner	=> restring_inner,
				restring_bottom	=> restring_bottom,
				upper			=> upper_layer);
					
			et_board_ops.vias.place_via (module_cursor, net_name, via, log_threshold + 1);
		end blind_bottom;

		
		procedure buried is
			via : type_via (BURIED);
		begin
			via := (drill with
				category		=> BURIED,
				restring_inner	=> restring_inner,
				layers			=> buried_layers);
					
			et_board_ops.vias.place_via (module_cursor, net_name, via, log_threshold + 1);
		end buried;

		
		rules : constant type_design_rules_board := get_pcb_design_rules (module_cursor);

		-- get the user specific settings of the board
		settings : constant et_pcb.type_user_settings := get_user_settings (module_cursor);

		
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

		
		case cmd_field_count is
			when 7 => 
				-- example: board demo place via RESET_N 10 14
				set_net_name;
				set_position;
				through;

				
			when 10 =>				
				if get_field (8) = keyword_buried then
					-- example: board demo place via RESET_N 10 14 buried 2 15					
					set_net_name;
					set_position;
					buried_layers := to_buried_layers (
								upper	=> get_field (9), 
								lower	=> get_field (10),
								bottom	=> get_deepest_conductor_layer (module_cursor));
					buried;

					
				elsif get_field (8) = keyword_blind then
					-- example: board demo place via RESET_N 10 14 blind top 5
					-- example: board demo place via RESET_N 10 14 blind bottom 2
					set_net_name;
					set_position;

					if get_field (9) = keyword_top then
						lower_layer := to_signal_layer (get_field (10));
						blind_top;
						
					elsif get_field (9) = keyword_bottom then
						upper_layer := to_signal_layer (get_field (10));
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
				
			when 11 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;

	end place_via;



	-- This procedure parses a command
	-- to move a via.
	-- CS: Currently this procedure does not many things.	
	procedure move_via is
	begin
		null;
	end move_via;
		
	

	-- This procedure parses a command
	-- to delete a via.
	-- CS: Currently this procedure does not many things.	
	procedure delete_via is
	begin
		null;
	end delete_via;



	

-- ROUTE / TRACK / POLYGON


	-- Parses a command like "board demo set zone fill solid/hatched"
	-- or "board demo set set zone isolation 0.4" and sets the value
	-- in user specific settings.
	-- CS: Do a plausibility check of zone parameters against each other.
	procedure set_fill_zone_properties is
		kw_log			: constant string := "log";
		
		use pac_generic_modules;
		use et_fill_zones;
		
		comma : constant character := ',';
		
		procedure expect_keywords is begin
			raise syntax_error_1 with 
				"ERROR: Expect keyword "
				& enclose_in_quotes (keyword_linewidth) & comma
				& enclose_in_quotes (keyword_connection) & comma
				& enclose_in_quotes (keyword_priority) & comma
				& enclose_in_quotes (keyword_spacing) & comma
				& enclose_in_quotes (keyword_relief) & comma
				& enclose_in_quotes (keyword_easing) & comma
				& enclose_in_quotes (keyword_isolation) & " or "
				& enclose_in_quotes (kw_log) 
				& " after keyword " & enclose_in_quotes (to_lower (to_string (noun))) & " !";
		end expect_keywords;


		procedure set_fill_style (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.fill_style := to_fill_style (get_field (6));
		end set_fill_style;

		
		procedure set_linewidth (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.linewidth := to_distance (get_field (6));
		end set_linewidth;

		
		procedure set_iso (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.isolation := to_distance (get_field (6));
		end set_iso;

		
		procedure set_priority (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.priority_level := to_priority (get_field (6));
		end set_priority;

		
		procedure set_easing_style (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.easing.style := to_easing_style (get_field (7));
		end set_easing_style;

		
		procedure set_easing_radius (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.easing.radius := to_distance (get_field (7));
		end set_easing_radius;	

		
		procedure set_connection (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.connection := to_pad_connection (get_field (6));
		end set_connection;	

		
		procedure set_hatching_spacing (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.spacing := to_distance (get_field (6));
		end set_hatching_spacing;	

		
		procedure set_thermal_width (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.thermal.width_min := to_distance (get_field (7));
		end set_thermal_width;	

		
		procedure set_thermal_gap (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.thermal.gap_max := to_distance (get_field (7));
		end set_thermal_gap;	

		
	begin -- set_fill_zone_properties
		case cmd_field_count is
			when 6 => 
				-- board demo set zone fill solid/hatched
				if get_field (5) = keyword_fill then
					update_element (generic_modules, module_cursor, set_fill_style'access);

				-- board demo set zone linewidth 0.25
				elsif get_field (5) = keyword_linewidth then
					update_element (generic_modules, module_cursor, set_linewidth'access);

				-- board demo set zone spacing 0.3
				elsif get_field (5) = keyword_spacing then
					update_element (generic_modules, module_cursor, set_hatching_spacing'access);
					
				-- board demo set zone isolaton 0.4
				elsif get_field (5) = keyword_isolation then
					update_element (generic_modules, module_cursor, set_iso'access);
					
				-- board demo set zone priority 2
				elsif get_field (5) = keyword_priority then
					update_element (generic_modules, module_cursor, set_priority'access);

				-- board demo set zone connection thermal/solid
				elsif get_field (5) = keyword_connection then
					update_element (generic_modules, module_cursor, set_connection'access);

				-- board demo set zone log NORMAL/HIGH/INSANE
				elsif get_field (5) = kw_log then
					polygon_log_category := to_log_category (get_field (6));
				else
					expect_keywords;
				end if;

				
			when 7 =>
				-- board demo set zone easing style none/chamfer/fillet
				if get_field (5) = keyword_easing then

					if get_field (6) = keyword_style then
						update_element (generic_modules, module_cursor, set_easing_style'access);

					elsif get_field (6) = keyword_radius then
						update_element (generic_modules, module_cursor, set_easing_radius'access);

					else
						raise syntax_error_1 with
							"ERROR: Expect keywords " 
							& enclose_in_quotes (keyword_style) & " or "
							& enclose_in_quotes (keyword_radius) 
							& " after keyword " & enclose_in_quotes (keyword_easing) & " !";
					end if;


				-- board demo set zone relief width_min/ gap_max 0.3
				elsif get_field (5) = keyword_relief then

					if get_field (6) = keyword_width_min then
						update_element (generic_modules, module_cursor, set_thermal_width'access);

					elsif get_field (6) = keyword_gap_max then
						update_element (generic_modules, module_cursor, set_thermal_gap'access);

					-- CS technology ?	
						
					else
						raise syntax_error_1 with
						"ERROR: Expect keywords " 
							& enclose_in_quotes (keyword_width_min) & " or "
							& enclose_in_quotes (keyword_gap_max) 
							& " after keyword " & enclose_in_quotes (keyword_relief) & " !";
					end if;

					
				else
					expect_keywords;
				end if;
				
			when 8 .. type_field_count'last => too_long;

			when others => command_incomplete;
				
		end case;
	end set_fill_zone_properties;


	
	
	type type_track_shape is (LINE, ARC, ZONE);
	-- CS circular tracks are currently not supported

	
	
	procedure route_freetrack is
		shape : constant type_track_shape := type_track_shape'value (get_field (6));

		-- get the user specific settings of the board
		settings : constant et_pcb.type_user_settings := get_user_settings (module_cursor);
		

		-- Extract from the given command the zone 
		-- arguments (everything after "zone"):
		procedure make_fill_zone is
			use et_fill_zones;
			
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			ps : type_floating_solid;
			ph : type_floating_hatched;

			-- Build a basic polygon from the arguments:
			p : constant type_contour := type_contour (to_contour (arguments));
		begin
			case settings.polygons_conductor.fill_style is
				when SOLID =>
			
					ps := (p with 
						fill_style	=> SOLID,
						linewidth	=> settings.polygons_conductor.linewidth,
						isolation	=> settings.polygons_conductor.isolation,
						properties	=> (
							layer 			=> to_signal_layer (get_field (5)),
							priority_level	=> settings.polygons_conductor.priority_level,
							others			=> <>),

						islands		=> no_islands,
						easing		=> settings.polygons_conductor.easing);

					add_zone (module_cursor, ps, log_threshold + 1);

					
				when HATCHED =>

					ph := (p with 
						fill_style	=> HATCHED,
						spacing		=> settings.polygons_conductor.spacing,
						linewidth	=> settings.polygons_conductor.linewidth,
						isolation	=> settings.polygons_conductor.isolation,
						properties	=> (
							layer 			=> to_signal_layer (get_field (5)),
							priority_level	=> settings.polygons_conductor.priority_level,
							others			=> <>),

						islands		=> no_islands,
						easing		=> settings.polygons_conductor.easing);

					add_zone (module_cursor, ph, log_threshold + 1);
					
			end case;
		end make_fill_zone;


		line_tmp	: type_line;
		arc_tmp		: type_arc;
		width_tmp	: type_distance_positive;
		layer_tmp	: type_signal_layer;

		
	begin -- route_freetrack
		case shape is
			when LINE =>
				case cmd_field_count is
					when 11 =>
						-- draw a freetrack
						layer_tmp := to_signal_layer (get_field (5));
						width_tmp := to_distance (get_field (7));

						line_tmp := type_line (to_line (
							A => to_vector_model (get_field (8), get_field (9)),
							B => to_vector_model (get_field (10), get_field (11))));
														   
						add_line (
							module_name 	=> module,
							net_name		=> to_net_name (""),
							line			=> (line_tmp with width_tmp, layer_tmp),							
							log_threshold	=> log_threshold + 1);

					when 12 .. type_field_count'last =>
						too_long;
						
					when others =>
						command_incomplete;
				end case;

				
			when ARC =>
				case cmd_field_count is
					when 14 =>
						layer_tmp := to_signal_layer (get_field (5));
						width_tmp := to_distance (get_field (7));

						arc_tmp := type_arc (to_arc (
							center		=> to_vector_model (get_field (8), get_field (9)),
							A			=> to_vector_model (get_field (10), get_field (11)),
							B			=> to_vector_model (get_field (12), get_field (13)),
							direction	=> to_direction (get_field (14))));
														
						-- draw a freetrack
						add_arc (
							module_name 	=> module,
							arc				=> (arc_tmp with width_tmp, layer_tmp),
							net_name		=> to_net_name (""),
							log_threshold	=> log_threshold + 1);
						
					when 15 .. type_field_count'last =>
						too_long;
						
					when others =>
						command_incomplete;
				end case;

				
			when ZONE =>
				case cmd_field_count is
					when 5 .. type_field_count'last =>
						make_fill_zone;

					when others =>
						command_incomplete;
				end case;
		end case;
	end route_freetrack;

	

	
	
	procedure route_net is 
		use et_terminals;
		shape : constant type_track_shape := type_track_shape'value (get_field (7));

		-- get the user specific settings of the board
		settings : constant et_pcb.type_user_settings := get_user_settings (module_cursor);

		
		procedure make_fill_zone is
			use et_fill_zones;
			
			-- Extract from the given command the polygon arguments (everything after "zone"):
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 7);

			-- Build a basic polygon from the arguments:
			p0 : constant type_contour := type_contour (to_contour (arguments));

			
			procedure make_solid_thermal is
				p1 : type_zone_solid;
				p2 : type_route_solid (connection => THERMAL);
			begin
				p1 := (p0 with 
					fill_style	=> SOLID,
					linewidth	=> settings.polygons_conductor.linewidth,
					isolation	=> settings.polygons_conductor.isolation,					
					islands		=> no_islands,
					easing		=> settings.polygons_conductor.easing);

				p2 := (p1 with
					connection			=> THERMAL,
					relief_properties	=> settings.polygons_conductor.thermal,
					properties		=> (
						layer 			=> to_signal_layer (get_field (6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						others			=> <>),
					others				=> <>);
					   
				add_zone (
					module_cursor	=> module_cursor,
					zone			=> p2,
					net_name		=> to_net_name (get_field (5)),
					log_threshold	=> log_threshold + 1);
				
			end make_solid_thermal;

			
			procedure make_solid_solid is 
				p1 : type_zone_solid;
				p2 : type_route_solid (connection => SOLID);
			begin
				p1 := (p0 with 
					fill_style	=> SOLID,
					linewidth	=> settings.polygons_conductor.linewidth,
					isolation	=> settings.polygons_conductor.isolation,
					islands		=> no_islands,
					easing		=> settings.polygons_conductor.easing);

				p2 := (p1 with
					connection	=> SOLID,
					technology	=> SMT_AND_THT, -- CS settings.polygons_conductor.technology,
					properties	=> (
						layer 			=> to_signal_layer (get_field (6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						others			=> <>),
					others				=> <>);
					   
				add_zone (
					module_cursor	=> module_cursor,
					zone			=> p2,
					net_name		=> to_net_name (get_field (5)),
					log_threshold	=> log_threshold + 1);

			end make_solid_solid;

			
			procedure make_hatched_thermal is
				p1 : type_zone_hatched;
				p2 : type_route_hatched (connection => THERMAL);
			begin
				p1 := (p0 with 
					fill_style	=> HATCHED,
					spacing		=> settings.polygons_conductor.spacing,
					linewidth	=> settings.polygons_conductor.linewidth,
					isolation	=> settings.polygons_conductor.isolation,
					islands		=> no_islands,			
					easing		=> settings.polygons_conductor.easing);

				p2 := (p1 with
					connection		=> THERMAL,
					relief_properties	=> settings.polygons_conductor.thermal,
					properties		=> (
						layer 			=> to_signal_layer (get_field (6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						others			=> <>),
					others				=> <>);
					   
				add_zone (
					module_cursor	=> module_cursor,
					zone			=> p2,
					net_name		=> to_net_name (get_field (5)),
					log_threshold	=> log_threshold + 1);
				
			end make_hatched_thermal;

			
			procedure make_hatched_solid is
				p1 : type_zone_hatched;
				p2 : type_route_hatched (connection => SOLID);
			begin
				p1 := (p0 with 
					fill_style	=> HATCHED,
					spacing		=> settings.polygons_conductor.spacing,
					linewidth	=> settings.polygons_conductor.linewidth,
					isolation	=> settings.polygons_conductor.isolation,
					islands		=> no_islands,
					easing		=> settings.polygons_conductor.easing);

				p2 := (p1 with
					connection	=> SOLID,
					technology	=> SMT_AND_THT, -- CS settings.polygons_conductor.technology,
					properties	=> (
						layer 			=> to_signal_layer (get_field (6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						others			=> <>),
					others				=> <>);
					   
				add_zone (
					module_cursor	=> module_cursor,
					zone			=> p2,
					net_name		=> to_net_name (get_field (5)),
					log_threshold	=> log_threshold + 1);
				
			end make_hatched_solid;

			
		begin -- make_fill_zone
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
		end make_fill_zone;


		use et_board_coordinates.pac_grid;

		line_tmp	: type_line;
		arc_tmp		: type_arc;
		width_tmp	: type_distance_positive;
		layer_tmp	: type_signal_layer;

		
	begin -- route_net
		case shape is
			when LINE =>
				if is_number (get_field (9)) then -- 33.4 or IC4
					
					-- THE TRACK STARTS AT A DEDICATED POINT AT X/Y:
					
					-- board motor_driver route net NET_1 2 line 0.25 0 0 160 0
					case cmd_field_count is
						when 12 =>
							layer_tmp := to_signal_layer (get_field (6));
							width_tmp := to_distance (get_field (8));
							
							line_tmp := type_line (to_line (
								A => to_vector_model (get_field  (9), get_field (10)),
								B => to_vector_model (get_field (11), get_field (12))));
														   
							add_line (
								module_name 	=> module,
								net_name		=> to_net_name (get_field (5)),
								line			=> (line_tmp with width_tmp, layer_tmp),								
								log_threshold	=> log_threshold + 1);

						when 13 .. type_field_count'last => too_long;
							
						when others => command_incomplete;
					end case;
					
				else
					-- THE TRACK STARTS AT A TERMINAL:
					
					if get_field (11) = keyword_to then
						-- board motor_driver route net NET_1 1 line 0.25 R1 1 to 35 40
						-- board motor_driver route net NET_1 1 line 0.25 R1 1 to x 5
						
						if is_number (get_field (12)) then
							-- THE TRACK ENDS AT A DEDICATED POINT X/Y
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 to 35 40
							case cmd_field_count is
								when 13 =>
									add_line (
										module_name 	=> module,
										net_name		=> to_net_name (get_field (5)),
										layer			=> to_signal_layer (get_field (6)),
										width			=> to_distance (get_field (8)),
										device			=> to_device_name (get_field (9)),
										terminal		=> to_terminal_name (get_field (10)),
										end_point		=> to_vector_model (get_field (12), get_field (13)), -- 35 40
										log_threshold	=> log_threshold + 1);
									
								when 14 .. type_field_count'last =>
									too_long;
									
								when others =>
									command_incomplete;
							end case;
									
						else
							-- THE TRACK ENDS ON A GRID LINE ALONG A GIVEN AXIS:
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 to x 5
							if get_field (12) = to_string (AXIS_X) or get_field (12) = to_string (AXIS_Y) then
								case cmd_field_count is
									when 13 =>
										add_line (
											module_name => module,
											net_name	=> to_net_name (get_field (5)),
											layer		=> to_signal_layer (get_field (6)),
											width		=> to_distance (get_field (8)),
											device		=> to_device_name (get_field (9)),
											terminal	=> to_terminal_name (get_field (10)),
											axis		=> to_axis (get_field (12)),
											notches		=> to_notches (get_field (13)), -- 5
											
											log_threshold	=> log_threshold + 1
											);
										
									when 14 .. type_field_count'last =>
										too_long;
										
									when others =>
										command_incomplete;
								end case;
								
							else
								invalid_keyword (12);
							end if;
						end if;
						
						
					elsif get_field (11) = keyword_direction then
						-- THE TRACK RUNS INTO GIVEN DIRECTION SPECIFIED BY AN ANGLE
						
						if is_number (get_field (13)) then
							-- THE TRACK ENDS AFTER A GIVEN DISTANCE (it has a given length)
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 direction 45 50
							
							case cmd_field_count is
								when 13 =>
									add_line (
										module_name => module,
										net_name	=> to_net_name (get_field (5)),
										layer		=> to_signal_layer (get_field (6)),
										width		=> to_distance (get_field (8)),
										device		=> to_device_name (get_field (9)),
										terminal	=> to_terminal_name (get_field (10)),
										direction	=> to_rotation (get_field (12)), -- 45 degree
										length		=> to_distance (get_field (13)), -- 50mm
										
										log_threshold	=> log_threshold + 1
										);

								when 14 .. type_field_count'last =>
									too_long;
									
								when others =>
									command_incomplete;
							end case;

						else
							-- THE TRACK ENDS AT A GIVEN GRID LINE ALONG A GIVEN AXIS
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 direction 45 x 5
							if get_field (13) = to_string (AXIS_X) or get_field (13) = to_string (AXIS_Y) then
								
								case cmd_field_count is
									when 14 =>
										add_line (
											module_name => module,
											net_name	=> to_net_name (get_field (5)),
											layer		=> to_signal_layer (get_field (6)),
											width		=> to_distance (get_field (8)),
											device		=> to_device_name (get_field (9)),
											terminal	=> to_terminal_name (get_field (10)),
											direction	=> to_rotation (get_field (12)), -- 45 degree
											axis		=> to_axis (get_field (13)),
											notches		=> to_notches (get_field (14)), -- 5
											
											log_threshold	=> log_threshold + 1
											);

									when 15 .. type_field_count'last => too_long;
										
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
				case cmd_field_count is
					when 15 =>
						arc_tmp := type_arc (to_arc (
							center		=> type_vector_model (set (
								x => to_distance (dd => get_field (9)),
								y => to_distance (dd => get_field (10)))),
							A	=> type_vector_model (set (
								x => to_distance (dd => get_field (11)),
								y => to_distance (dd => get_field (12)))),
							B	=> type_vector_model (set (
								x => to_distance (dd => get_field (13)),
								y => to_distance (dd => get_field (14)))),
							direction	=> to_direction (get_field (15))));
														
						-- draw a named track
						add_arc (
							module_name => module,
							net_name	=> to_net_name (get_field (5)),
							arc			=> (arc_tmp with
								layer	=> to_signal_layer (get_field (6)),
								width	=> to_distance (get_field (8))),
							log_threshold	=> log_threshold + 1
							);
						
					when 16 .. type_field_count'last => too_long;
						
					when others =>
						command_incomplete;
				end case;

				
			when ZONE =>
				case cmd_field_count is
					-- The polygon command is very long. The following example spreads across
					-- several lines:
					--  board led_driver route net RESET_N 1 zone /
					--  line 0 0 100 0 /
					--  line 100 0 100 100 / 
					--  arc 50 100 100 100 0 100 ccw / 
					--  line 0 100 0 0
					when 6 .. type_field_count'last =>
						make_fill_zone;

					when others =>
						command_incomplete;
				end case;
				
		end case;
	end route_net;



	
	-- This procedure parses a command to
	-- add a new signal layer:
	procedure add_signal_layer is

		procedure do_it is
			use et_pcb_stack;
			layer : type_layer;
		begin
			layer.conductor.thickness := to_distance (get_field (5));
			layer.dielectric.thickness := to_distance (get_field (6));
			
			add_layer (
				module_name 	=> module,
				layer			=> layer,
				log_threshold	=> log_threshold + 1);
		end do_it;
		

	begin
		case cmd_field_count is
			when 6 =>
				do_it;
				-- example: board tree_1 add layer 0.035 0.2

			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;		
	end add_signal_layer;

	


	-- This procedure parses a command to
	-- delete a signal layer:
	procedure delete_signal_layer is

		procedure do_it is begin
		
			delete_layer (
				module_name 	=> module,
				layer			=> to_signal_layer (get_field (5)),									
				log_threshold	=> log_threshold + 1);

		end do_it;

	begin
		case cmd_field_count is
			when 5 =>
				do_it;
				-- example: board tree_1 delete layer 2

			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_signal_layer;

	
	

	-- This procedure parses a command to add
	-- a non-electric device:
	procedure add_device is

		procedure do_it is
			use et_board_ops.devices;
			use et_package_names;
			use et_device_prefix;
			
			model : constant pac_package_model_file_name.bounded_string := to_file_name (get_field (5));
			prefix : constant pac_device_prefix.bounded_string := to_prefix (get_field (6));

			xy : constant type_vector_model := type_vector_model (set (
					x => to_distance (dd => get_field (7)),
					y => to_distance (dd => get_field (8))));

		begin
			case cmd_field_count is
				when 8 =>
					add_device (
						module_cursor	=> active_module,
						package_model	=> model,
						position		=> to_package_position
							(
							point	=> xy
							),
						prefix			=> prefix,
						log_threshold	=> log_threshold + 1);

					
				when 9 =>
					add_device (
						module_cursor	=> active_module,
						package_model	=> model,
						position		=> to_package_position
							(
							point		=> xy,
							rotation	=> to_rotation (get_field (9))
							),
						prefix			=> prefix,
						log_threshold	=> log_threshold + 1);

					
				when 10 =>
					add_device (
						module_cursor	=> active_module,
						package_model	=> model,
						position		=> to_package_position
							(
							point		=> xy,
							rotation	=> to_rotation (get_field (9)),
							face		=> to_face (get_field (10))
							),
						prefix			=> prefix,
						log_threshold	=> log_threshold + 1);
					
				when others => raise constraint_error; -- CS should never happen
			end case;
		end do_it;

		
	begin
		case cmd_field_count is
			when 8..10 => do_it;
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0 top

			when 11 .. type_field_count'last => too_long;
			
			when others => command_incomplete;
		end case;
	end add_device;
	



	
	-- This procedure parses a command to delete
	-- a non-electrical device:
	procedure delete_device is
		use et_board_ops.devices;

		
		procedure do_it is begin
			delete_device (
				module_cursor	=> active_module,
				device_name		=> to_device_name (get_field (5)),
				log_threshold	=> log_threshold + 1);
		end do_it;
		

	begin
		case cmd_field_count is
			when 5 => do_it;
				-- example: board led_driver delete device FD1
			
			when 6 .. type_field_count'last => too_long;
			
			when others => command_incomplete;
		end case;		
	end delete_device;


	

	procedure move_device is begin
		case cmd_field_count is
			when 8 =>
				et_board_ops.devices.move_device (
					module_cursor 	=> active_module,
					device_name		=> to_device_name (get_field (5)), -- IC1
					coordinates		=> to_coordinates (get_field (6)),  -- relative/absolute
					point			=> type_vector_model (set (
										x => to_distance (dd => get_field (7)),
										y => to_distance (dd => get_field (8)))),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last =>
				too_long;
				
			when others =>
				command_incomplete;
		end case;
	end move_device;


	
	
	-- This procedure parses a command to
	-- rename a non-electrical device:
	procedure rename_device is		
		use et_board_ops.devices;

		
		procedure do_it is begin
			rename_device (
				module_cursor		=> active_module,
				device_name_before	=> to_device_name (get_field (5)),
				device_name_after	=> to_device_name (get_field (6)),
				log_threshold		=> log_threshold + 1);

		end do_it;

		
	begin
		case cmd_field_count is
			when 6 => do_it; 
			-- exampe: board led_driver rename device FD1 FD3

			when 7 .. type_field_count'last => too_long;
			
			when others => command_incomplete;
		end case;
	end rename_device;




	procedure flip_device is begin
		case cmd_field_count is
			when 6 =>
				et_board_ops.devices.flip_device (
					module_cursor 	=> active_module,
					device_name		=> to_device_name (get_field (5)), -- IC1
					face			=> to_face  (get_field (6)),  -- top/bottom
					log_threshold	=> log_threshold + 1
					);

			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end flip_device;

	
	
	

	-- This procedure parses a command to 
	-- delete an object of the silkscreen:
	procedure delete_silkscreen_object is
		
		procedure do_it is
			use et_board_ops.silkscreen;
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (6), get_field (7)),
				radius	=> to_zone_radius (get_field (8)));
				
			delete_object (
				module_name 	=> module,
				face			=> to_face (get_field (5)),
				catch_zone		=> catch_zone,
				log_threshold	=> log_threshold + 1);
			
		end do_it;
				
	begin
		case cmd_field_count is
			-- example: board led_driver delete silkscreen top 40 50 1
			when 8 => do_it;
			when 9 .. type_field_count'last => too_long;				
			when others => command_incomplete;
		end case;
	end delete_silkscreen_object;

	

	
	-- This procedure parses a command to 
	-- delete an object of the assembly documentation:
	procedure delete_assy_doc_object is

		procedure do_it is
			use et_board_ops.assy_doc;
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (6), get_field (7)),
				radius	=> to_zone_radius (get_field (8)));
				
			delete_object (
				module_name 	=> module,
				face			=> to_face (get_field (5)),
				catch_zone		=> catch_zone,
				log_threshold	=> log_threshold + 1);

		end do_it;

	begin
		case cmd_field_count is
			-- example: board led_driver delete assy top 40 50 1
			when 8 => do_it;
			when 9 .. type_field_count'last => too_long;				
			when others => command_incomplete;
		end case;		
	end delete_assy_doc_object;
	
	

	-- This procedure parses a command to 
	-- delete an object in a keepout layer:
	procedure delete_keepout_object is
	begin
		-- board led_driver delete keepout top 40 50 1
		null; -- CS
	end delete_keepout_object;



	
	-- This procedure parses a command to 
	-- delete an object in a stencil layer:
	procedure delete_stencil_object is

		procedure do_it is
			use et_board_ops.stencil;
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (6), get_field (7)),
				radius	=> to_zone_radius (get_field (8)));

			delete_object (
				module_name 	=> module,
				face			=> to_face (get_field (5)),
				catch_zone		=> catch_zone,
				log_threshold	=> log_threshold + 1);

		end do_it;
		
	begin
		-- board led_driver delete stencil top 40 50 1
		case cmd_field_count is
			when 8 => do_it;
			when 9 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
	end delete_stencil_object;
	



	-- This procedure parses a command to 
	-- delete an object in a stopmask layer:
	procedure delete_stopmask_object is

		procedure do_it is
			use et_board_ops.stopmask;
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (6), get_field (7)),
				radius	=> to_zone_radius (get_field (8)));
				
			delete_object (
				module_name 	=> module,
				face			=> to_face (get_field (5)),
				catch_zone		=> catch_zone,				
				log_threshold	=> log_threshold + 1);

		end do_it;
		
	begin
		-- board led_driver delete stop top 40 50 1
		case cmd_field_count is
			when 8 => do_it;
			when 9 .. type_field_count'last => too_long;				
			when others => command_incomplete;
		end case;
	end delete_stopmask_object;
	



	-- This procedure parses a command to 
	-- delete an object in a route restrict layer:
	procedure delete_route_restrict_object is

		procedure do_it is
			use et_board_ops.route_restrict;
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (5), get_field (6)),
				radius	=> to_zone_radius (get_field (7)));
											 
			delete_route_restrict (
				module_name 	=> module,
				catch_zone		=> catch_zone,
				log_threshold	=> log_threshold + 1);

		end do_it;
		
	begin
		-- board led_driver delete route_restrict 40 50 1
		case cmd_field_count is
			when 7 => do_it;
			when 8 .. type_field_count'last => too_long;								
			when others => command_incomplete;
		end case;
	end delete_route_restrict_object;
	

	


	-- This procedure parses a command to 
	-- delete an object in a via restrict layer:
	procedure delete_via_restrict_object is

		procedure do_it is
			use et_board_ops.via_restrict;
		begin
			-- CS delete segment of zone contour 
			null;
-- 			delete_via_restrict (
-- 				module_name 	=> module,
-- 				point			=> type_vector_model (set (
-- 						x => to_distance (dd => get_field (5)),
-- 						y => to_distance (dd => get_field (6)))),
-- 				accuracy		=> to_accuracy (get_field (7)),
-- 				
-- 				log_threshold	=> log_threshold + 1);
		end do_it;
		
	begin
		-- board led_driver delete via_restrict 40 50 1
		case cmd_field_count is
			when 7 => do_it;
			when 8 .. type_field_count'last => too_long;				
			when others => command_incomplete;
		end case;
	end delete_via_restrict_object;


	

	
	-- This procedure parses a command to 
	-- delete an freetrack segment in a conductor layer:
	procedure delete_freetrack_segment is
	
		procedure do_it is 
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (6), get_field (7)),
				radius	=> to_zone_radius (get_field (8)));
				
			delete_track (
				module_name 	=> module,
				net_name		=> to_net_name (""),
				layer			=> to_signal_layer (get_field (5)),
				catch_zone		=> catch_zone,
				log_threshold	=> log_threshold + 1);

		end do_it;
		
	begin
		case cmd_field_count is
			when 8 => do_it;
			when 9 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
	end delete_freetrack_segment;

	

	

	-- This procedure parses a command to
	-- delete a segment of a net in a conductor layer:
	procedure delete_net_segment is

		procedure do_it is 
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (7), get_field (8)),
				radius	=> to_zone_radius (get_field (9)));
		
			delete_track (
				module_name 	=> module,
				net_name		=> to_net_name (get_field (5)),
				layer			=> to_signal_layer (get_field (6)),
				catch_zone		=> catch_zone,				
				log_threshold	=> log_threshold + 1);

		end do_it;

	begin
		case cmd_field_count is
			when 9 => do_it;
			when 10 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
	end delete_net_segment;
	


	

	procedure fill_zones is 
		nets : pac_net_names.list;
	begin
		case cmd_field_count is
			when 4 => -- fill all zones
				
				-- command: board demo fill zone
				fill_zones (module_cursor, polygon_log_category, log_threshold + 1);

				
			when others => 
				-- like: board demo fill zone GND P3V3 AGND

				-- collect the optional net names in list "nets":
				for place in 5 .. cmd_field_count loop
					nets.append (to_net_name (get_field (place)));
				end loop;

				fill_zones (module_cursor, polygon_log_category, log_threshold + 1, nets);
		end case;
				
		if runmode /= MODE_HEADLESS then
			set_status ("conductor zones filled");
		end if;
	end fill_zones;
	


	
	procedure clear_fill_zone is
		nets : pac_net_names.list;
	begin
		case cmd_field_count is
			when 4 => -- clear all zones
				
				-- command: board demo clear zone
				clear_zones (module_cursor, log_threshold + 1);

				
			when others => 
				-- like: board demo clear zone GND P3V3 AGND

				-- collect the optional net names in list "nets":
				for place in 5 .. cmd_field_count loop
					nets.append (to_net_name (get_field (place)));
				end loop;

				clear_zones (module_cursor, log_threshold + 1, nets);
		end case;
					
		if runmode /= MODE_HEADLESS then
			set_status ("conductor zones cleared");
		end if;
	end clear_fill_zone;


	

	procedure update_ratsnest is
		use et_board_ops.ratsnest;
	begin
		-- board demo update ratsnest
		case cmd_field_count is
				
			when 4 => 
				update_ratsnest (module_cursor, log_threshold + 1);
				set_status (status_ratsnest_updated);
			
			when 5 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end update_ratsnest;


	
	
	procedure move_drawing_frame is 
		use et_board_ops.frame;
		use pac_drawing_frame;

		p : et_drawing_frame.type_position;
		c : type_coordinates;
	begin
		case cmd_field_count is
			when 7 => -- board led_driver move frame absolute -20 -50
				c := to_coordinates (get_field (5));   -- relative/absolute
				
				p.x := et_drawing_frame.to_distance (get_field (6));
				p.y := et_drawing_frame.to_distance (get_field (7));

				move_drawing_frame (
					module_cursor 	=> module_cursor,
					coordinates		=> c,
					point			=> p,
					log_threshold	=> log_threshold + 1
					);

				
			when 8 .. type_field_count'last =>
				too_long;
				
			when others =>
				command_incomplete;
		end case;
	end move_drawing_frame;


	

	-- Actions to save a module:
	procedure save_module is 
	begin
		-- Since we are already in the project directory,
		-- we can call the save_module procedures right away.
		
		case cmd_field_count is
			when 4 =>
				-- Save the module with its own name:
				save_module (
					module_cursor	=> active_module,
					log_threshold	=> log_threshold + 1);

			when 5 =>
				-- Save the module with a different name:
				save_module (
					module_cursor	=> active_module,
					save_as_name	=> to_module_name (get_field (5)), -- led_driver_test
					log_threshold	=> log_threshold + 1);
				
			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;			

	end save_module;
	


	-- This procedure extracts from the command the
	-- name of the generic module and optionally the
	-- sheet number.
	-- It sets the given module and sheet as active
	-- and updates the editor window according
	-- to the activated module:
	procedure show_module is  -- GUI related

		module : pac_module_name.bounded_string;

		use et_sheets;
		sheet : type_sheet := 1;

		
		-- Sets the active module and first sheet.
		procedure module_and_first_sheet is 
			use et_canvas_schematic_2;
			use et_schematic_coordinates;
		begin
			module := to_module_name (get_field (5));
			set_module (module);
			active_sheet := sheet;
			
			et_canvas_schematic_2.update_schematic_editor;
			et_canvas_board_2.update_board_editor;
		end module_and_first_sheet;



		-- Sets the active module and sheet.
		procedure module_and_random_sheet is 
			use et_canvas_schematic_2;
			use et_schematic_coordinates;
		begin
			module := to_module_name (get_field (5));
			set_module (module);

			log (text => "sheet " & to_string (sheet), 
				level => log_threshold + 1);
			
			sheet := to_sheet (get_field (6));
			active_sheet := sheet;

			et_canvas_schematic_2.update_schematic_editor;
			et_canvas_board_2.update_board_editor;
		end module_and_random_sheet;
		
		
	begin
		log (text => "show module (via board editor) " 
			 & enclose_in_quotes (to_string (module)),
			 level => log_threshold + 1);

		case cmd_field_count is
			when 5 => module_and_first_sheet; -- show module LED-driver
			when 6 => module_and_random_sheet; -- show module LED-driver 2
			when 7 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
		
	end show_module;




	procedure execute_script is begin
		case cmd_field_count is
			when 5 => 

				execute_nested_script (
					file			=> get_field (5),
					log_threshold	=> log_threshold + 1);

			when 6 .. type_field_count'last => too_long;								
			when others => command_incomplete;
		end case;
	end execute_script;



	
	
	-- Parses the single_cmd.fields:
	procedure parse is 
	begin
		log (text => "parsing command: " 
			& enclose_in_quotes (get_all_fields (cmd)),
			level => log_threshold);

		-- Clear the status bar if we are in graphical mode:
		if runmode /= MODE_HEADLESS then
			status_clear;
		end if;
		
		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_DEVICE =>
						add_device;

					when NOUN_LAYER =>
						add_signal_layer;

					when others => invalid_noun (to_string (noun));
				end case;


			when VERB_CLEAR =>
				case noun is
					when NOUN_ZONE =>
						clear_fill_zone;

					when others => invalid_noun (to_string (noun));
				end case;

						
			when VERB_DELETE =>
				case noun is
					when NOUN_DEVICE =>
						delete_device;						

					when NOUN_LAYER =>
						delete_signal_layer;						

					when NOUN_HOLE =>
						delete_hole_segment;

					when NOUN_OUTLINE =>
						delete_outline_segment;
						
					when NOUN_SILKSCREEN =>
						delete_silkscreen_object;

					when NOUN_ASSY =>
						delete_assy_doc_object;
						
					when NOUN_KEEPOUT =>
						delete_keepout_object;
						
					when NOUN_STENCIL =>
						delete_stencil_object;
						
					when NOUN_STOPMASK =>
						delete_stopmask_object;

					when NOUN_VIA =>
						delete_via;
						
					when NOUN_ROUTE_RESTRICT =>
						delete_route_restrict_object;						

					when NOUN_VIA_RESTRICT =>
						delete_via_restrict_object;

					when NOUN_FREETRACK =>
						delete_freetrack_segment;

					when NOUN_TRACK =>
						delete_net_segment;
						
					when others => invalid_noun (to_string (noun));

				end case;

				
			when VERB_DISPLAY => -- GUI related
				case noun is
					when NOUN_SILKSCREEN
						| NOUN_ASSY | NOUN_KEEPOUT | NOUN_STOPMASK | NOUN_STENCIL | NOUN_ORIGINS =>
						display_non_conductor_layer;

						
					when NOUN_CONDUCTORS =>
						display_conductor_layer;

					when NOUN_OUTLINE =>
						display_outline;

					when NOUN_RATSNEST =>
						display_ratsnest;

						
					when NOUN_RESTRICT => -- like "board led_driver display restrict route/via 2 [on/off]"
						case cmd_field_count is
							when 6 => display_restrict_layer (get_field (5), get_field (6)); -- if status is omitted
							when 7 => display_restrict_layer (get_field (5), get_field (6), get_field (7));
							when 8 .. type_field_count'last => too_long;
							when others => command_incomplete;
						end case;

					when NOUN_VIAS => -- like "board led_driver display vias [on/off]"
						case cmd_field_count is
							--when 5 => display_vias (get_field (5)); -- if status is omitted
							--when 6 => display_vias (get_field (5), get_field (6));
							--when 7 .. type_field_count'last => too_long;
							when 4 => display_vias; -- if status is omitted
							when 5 => display_vias (get_field (5));
							when 6 .. type_field_count'last => too_long;
							when others => command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_DRAW =>
				case noun is
					when NOUN_HOLE =>
						draw_hole;
						
					when NOUN_OUTLINE =>
						draw_board_outline;

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
						
					when NOUN_STOPMASK =>
						draw_stop_mask;

					when NOUN_VIA_RESTRICT =>
						draw_via_restrict;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_EXECUTE =>
				case noun is
					when NOUN_SCRIPT =>
						execute_script;
							
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_EXIT | VERB_QUIT => 
				null;
				-- CS terminate_main; 
				-- CS does not work via script (gtk error ...)

				
			when VERB_FILL =>
				case noun is
					when NOUN_ZONE =>
						fill_zones;
						
					when others => 
						invalid_noun (to_string (noun));
				end case;

				
			when VERB_FLIP =>
				case noun is
					when NOUN_DEVICE =>
						flip_device;

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_MAKE =>
				case noun is
					when NOUN_PNP =>
						case cmd_field_count is
							when 4 =>
								make_pick_and_place 
									(
									module_name 	=> module,
									log_threshold	=> log_threshold + 1);

							when 5 .. type_field_count'last =>
								too_long;
								
							when others =>
								command_incomplete;
						end case;

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_MOVE =>
				case noun is
					when NOUN_FRAME =>
						move_drawing_frame;
						
					when NOUN_CURSOR =>
						parse_canvas_command (cmd, VERB_MOVE, NOUN_CURSOR);
						
					when NOUN_DEVICE =>
						move_device;

					when NOUN_SUBMODULE =>
						case cmd_field_count is
							when 8 =>
								move_submodule (
									module_name 	=> module,
									instance		=> to_instance_name (get_field (5)), -- OSC1
									coordinates		=> to_coordinates (get_field (6)),  -- relative/absolute
									point			=> type_vector_model (set (
														x => to_distance (dd => get_field (7)),
														y => to_distance (dd => get_field (8)))),
									log_threshold	=> log_threshold + 1
									);

							when 9 .. type_field_count'last =>
								too_long;
								
							when others =>
								command_incomplete;
						end case;

					when NOUN_VIA =>
						move_via;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_PLACE =>
				case noun is
					when NOUN_VIA			=> place_via;
					when NOUN_TEXT			=> place_text;
					when NOUN_PLACEHOLDER	=> place_text_placeholder;					
					when others	=> invalid_noun (to_string (noun));
				end case;
				

			when VERB_RENAME =>
				case noun is
					when NOUN_DEVICE =>
						rename_device;

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

			
			when VERB_ROTATE =>
				case noun is
					when NOUN_DEVICE =>
						case cmd_field_count is
							when 7 =>
								et_board_ops.devices.rotate_device (
									module_cursor 	=> active_module,
									device_name		=> to_device_name (get_field (5)), -- IC1
									coordinates		=> to_coordinates (get_field (6)),  -- relative/absolute
									rotation		=> to_rotation (get_field (7)),
									log_threshold	=> log_threshold + 1
									);

							when 8 .. type_field_count'last =>
								too_long;
								
							when others =>
								command_incomplete;
						end case;

					when others => invalid_noun (to_string (noun));
				end case;


			when VERB_SAVE =>
				case noun is
					when NOUN_MODULE =>
						save_module;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_SET =>
				case noun is
					when NOUN_GRID =>
						set_grid;
						
					when NOUN_CURSOR =>
						parse_canvas_command (cmd, VERB_SET, NOUN_CURSOR);

					when NOUN_ZOOM =>
						parse_canvas_command (cmd, VERB_SET, NOUN_ZOOM);

					when NOUN_SCALE =>
						set_scale;						
						
					when NOUN_ZONE =>
						set_fill_zone_properties; -- conductor layers related
						
					when NOUN_VIA =>
						set_via_properties;

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_SHOW => -- GUI related
				case noun is
					when NOUN_MODULE =>
						show_module;

						
	-- 					when NOUN_DEVICE =>
	-- 						case cmd_field_count is
	-- 							when 5 => null; -- CS
	-- 							when 6 .. type_field_count'last => too_long;
	-- 							when others => command_incomplete;
	-- 						end case;
							
				when others => invalid_noun (to_string (noun));
			end case;


			when VERB_UPDATE =>
				case noun is
					when NOUN_RATSNEST => -- board demo update ratsnest
						update_ratsnest;

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_ZOOM => -- GUI related
				zoom_all;

			when others =>
				null;
				
		end case;

		
		-- Update GUI if we are in graphical mode:
		if runmode /= MODE_HEADLESS then
			null;
		end if;



		
		exception
			when event: others =>
				-- log (text => ada.exceptions.exception_information (event), console => true);
				log (text => ada.exceptions.exception_information (event));
		
	end parse;


	
	

	-- This procedure proposes missing arguments:
	procedure propose_arguments is
		use et_canvas_board_devices;
		use et_canvas_board_texts;
		use et_canvas_board_vias;
		use et_canvas_board_preliminary_object;
		
		device_name : type_device_name;

		
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

		-- Missing arguments are to be proposed only if
		-- the command origin is the console.
		-- Otherwise nothing happens here.
		if not is_complete (cmd) and get_origin (cmd) = ORIGIN_CONSOLE then
		
			log_command_incomplete (cmd_field_count, log_threshold);

			case verb is
				when VERB_PLACE =>
					case noun is
						when NOUN_TEXT =>
							show_text_properties;
							set_finalization_pending (cmd);

						when NOUN_VIA =>
							case cmd_field_count is
								when 4 => -- place via
									show_via_properties;
									set_finalization_pending (cmd);

								when 5 => -- place via RESET_N
									-- Preset the net name so that it is visible
									-- in the via properties bar:
									object_net_name := to_net_name (get_field (5));

									show_via_properties;
									set_finalization_pending (cmd);

								when others => null;
							end case;
							
						when others => null; -- CS
					end case;

				when others => null; -- CS
			end case;
			
		end if;
	end propose_arguments;

	


	
begin -- board_cmd
	
	log (text => "execute board command: " & enclose_in_quotes (get_all_fields (cmd)),
		 level => log_threshold);
	

	module := to_module_name (get_field (2)); -- motor_driver (without extension *.mod)
	-- CS: Becomes obsolete once all board ops use the
	-- given module_cursor.


	set_verb_and_noun;
	

	-- Once verb and noun are known, they must be shown
	-- in the verb-noun-display:
	update_verb_noun_display;
	
	
	-- parse the command:
	parse;


	-- If the command is incomplete and if it was entered
	-- via the console, then further arguments are proposed.
	-- Otherwise nothing happens here:
	propose_arguments;


	log (text => "exit code" & natural'image (get_exit_code (cmd)), level => log_threshold);

	
	-- After every command (regardless if it is complete or not)
	-- set the focus to the canvas:
	-- CS: remove ?
	-- if runmode /= MODE_HEADLESS then
	-- 	canvas.grab_focus; -- NOTE ! calls "cb_draw"
	-- end if;


	-- exception when event: others =>

			-- null;
		-- CS
			-- evaluate_exception (
			-- 	name	=> exception_name (event),
			-- 	message	=> exception_message (event));

			-- raise;
	
end execute_board_command;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

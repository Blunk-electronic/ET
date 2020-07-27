------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        SCRIPTING IN SCHEMATIC                            --
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

with et_modes.schematic;

separate (et_scripting)
	
function schematic_cmd (
	cmd				: in type_fields_of_line; -- "schematic motor_driver draw net motor_on 1 150 100 150 130"
	log_threshold	: in type_log_level)
	return type_exit_code is

	use et_project;
	use et_schematic_ops;
	use et_coordinates;
	use pac_geometry_sch;
	use et_devices;
	use et_canvas_schematic.pac_canvas;
	use et_display.schematic;
	use et_modes.schematic;

	-- The exit code will be overridden with ERROR or WARNING if something goes wrong:
	exit_code : type_exit_code := SUCCESSFUL;
	
	domain	: type_domain; -- DOM_SCHEMATIC
	module	: type_module_name.bounded_string; -- motor_driver (without extension *.mod)
	
	verb : type_verb;
	noun : type_noun_schematic;

	
	function f (place : in positive) return string is begin
		return et_string_processing.field (cmd, place);
	end;

	function fields return count_type is begin
		return et_string_processing.field_count (cmd);
	end;

	procedure too_long is begin -- CS use it more often
		command_too_long (cmd, fields - 1);
	end;

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
	procedure position_cursor is  -- GUI related
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

	-- CS unify procedures show_unit and show_first_unit. They differ only in 
	-- the way the unit_name is assigned.
	
	-- Locates the unit of the given device.
	procedure show_unit is -- GUI related
		use et_devices;
		use et_canvas_schematic;
		
		device_name : et_devices.type_name := to_name (f (5)); -- IC45
		unit_name	: et_devices.type_unit_name.bounded_string := to_name (f (6)); -- C
		
		-- Locate the requested device and unit.
		location : type_unit_query := unit_position (
				module_cursor	=> current_active_module,
				device_name		=> device_name,
				unit_name		=> unit_name);
	begin
		-- CS log message ?
		
		if location.exists then
			-- show the sheet where the unit is:
			current_active_sheet := sheet (location.position);

			-- center on the unit
			center_on (canvas, type_point (location.position));
		end if;
		
		log (text => to_string (device_name, unit_name, location), console => true);
	end show_unit;

	-- Locates the one and only unit of the given device.
	procedure show_first_unit is -- GUI related
		use et_devices;
		use et_canvas_schematic;
		
		device_name : et_devices.type_name := to_name (f (5)); -- IC45

		-- The assumption is that the device has only one unit:
		unit_name	: et_devices.type_unit_name.bounded_string := to_name ("");
		
		-- Locate the requested device and unit.
		location : type_unit_query := unit_position (
				module_cursor	=> current_active_module,
				device_name		=> device_name,
				unit_name		=> unit_name);
	begin
		-- CS log message ?
		
		if location.exists then
			-- show the sheet where the unit is:
			current_active_sheet := sheet (location.position);

			-- center on the unit
			center_on (canvas, type_point (location.position));
		end if;

		log (text => to_string (device_name, unit_name, location), console => true);
	end show_first_unit;

	procedure show_sheet is -- GUI related
		use et_canvas_schematic;
		sheet : et_coordinates.type_sheet := to_sheet (f (5));
	begin
		log (text => "set sheet" & to_sheet (sheet), level => log_threshold + 1); 

		-- CS test if sheet exists
		
		current_active_sheet := sheet;

		-- Update module name and sheet number in the schematic window title bar:
		set_title_bar (active_module, current_active_sheet);
	end show_sheet;

	-- Sets the active module and first sheet.
	procedure show_module is -- GUI related
		use et_general;
		use et_project;
		use et_canvas_schematic;
		
		module : type_module_name.bounded_string := to_module_name (f (5));
	begin
		log (text => "set module " & enclose_in_quotes (to_string (module)), level => log_threshold + 1);
		set_module (module);
		current_active_sheet := 1;

		-- Update module name and sheet number in the schematic window title bar:
		set_title_bar (module, current_active_sheet);
		
		-- Update the board window title bar:
		et_canvas_board.set_title_bar (module);
	end show_module;

	procedure show_module_and_sheet is  -- GUI related
	-- Sets the active module and sheet.
		use et_general;
		use et_canvas_schematic;
		
		module : type_module_name.bounded_string := to_module_name (f (5));
		sheet : et_coordinates.type_sheet := to_sheet (f (6));
	begin
		log (text => "set module " & enclose_in_quotes (to_string (module))
			& " sheet " & to_sheet (sheet), level => log_threshold + 1);
		set_module (module);
		current_active_sheet := sheet;

		-- Update module name and sheet number in the schematic window title bar:
		set_title_bar (module, current_active_sheet);
		
		-- Update the board window title bar:
		et_canvas_board.set_title_bar (module);
	end show_module_and_sheet;

	-- Enables a certain layer. If status is empty, the layer will be enabled.
	procedure display ( -- GUI related
		layer	: in type_noun_schematic;
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
		
		log (text => "display " & to_lower (to_string (layer)) 
				& space & to_string (ls),
				level => log_threshold + 1);
		
		case layer is
			when NOUN_GRID		=> layers.grid := ls;
			when NOUN_NAMES		=> layers.device_names := ls;
			when NOUN_NETS		=> layers.nets := ls;
			when NOUN_PORTS		=> layers.ports := ls;
			when NOUN_PURPOSES	=> layers.device_purposes := ls;
			when NOUN_TEXTS		=> layers.texts := ls;
			when NOUN_VALUES	=> layers.device_values := ls;
			
			when others => 
				log (importance => ERROR, text => "invalid layer !", console => true);
		end case;

		-- CS exception handler if status is invalid
	end display;
	
begin -- schematic_cmd
	log (text => "full command: " & enclose_in_quotes (to_string (cmd)), level => log_threshold);

	domain := to_domain (f (1)); -- DOM_SCHEMATIC
	module := to_module_name (f (2)); -- motor_driver (without extension *.mod)

	-- read the verb from field 3
	verb := to_verb (f (3));

	-- The current operation mode is the same as the verb (both are of same type):
	op_mode := verb;

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
						when 9 =>
							-- If a virtual device is added, then no variant is required.
							add_device (
								module_name 	=> module,
								device_model	=> to_file_name (f (5)),
								destination		=> to_position 
									(
									sheet => to_sheet (f (6)),
									point => type_point (set 
												(
												x => to_distance (f (7)),
												y => to_distance (f (8))
												)),
									rotation => to_rotation (f (9))
									),
								variant			=> to_name (""),
								log_threshold	=> log_threshold + 1
								);

						when 10 =>
							-- A real device requires specification of a package variant.
							add_device (
								module_name 	=> module,
								device_model	=> to_file_name (f (5)),
								destination		=> to_position 
									(
									sheet => to_sheet (f (6)),
									point => type_point (set 
												(
												x => to_distance (f (7)),
												y => to_distance (f (8))
												)),
									rotation		=> to_rotation (f (9))
									),
								variant			=> to_name (f (10)),
								log_threshold	=> log_threshold + 1
								);

						when 11 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_NETCHANGER =>
					case fields is
						when 8 =>
							add_netchanger (
								module_name 	=> module,
								place			=> to_position 
									(
									sheet => to_sheet (f (5)),
									point => type_point (set 
												(
												x => to_distance (f (6)),
												y => to_distance (f (7))
												)),
									rotation		=> to_rotation (f (8))
									),
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_PORT =>
					case fields is
						when 9 =>
							add_port (
								module_name 	=> module,
								instance		=> et_general.to_instance_name (f (5)),
								port_name		=> et_general.to_net_name (f (6)),
								position		=> type_point (set 
											(
											x => to_distance (f (7)),
											y => to_distance (f (8))
											)),
								direction		=> et_submodules.to_port_name (f (9)),
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_SUBMODULE =>
					case fields is
						when 11 =>
							add_submodule (
								module_name 	=> module, -- parent module (where the submodule is to be inserted)
								file			=> et_submodules.to_submodule_path (f (5)),
								instance		=> et_general.to_instance_name (f (6)), -- submodule instance name
								position		=> to_position 
									(
									sheet => to_sheet (f (7)),
									point => type_point (set 
												(
												x => to_distance (f (8)),
												y => to_distance (f (9))
												))
									),
								size => (
									x => to_distance (f (10)),
									y => to_distance (f (11))
									),
								log_threshold	=> log_threshold + 1
								);

						when 12 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_BUILD =>
			case noun is
				when NOUN_SUBMODULES_TREE =>
					case fields is
						when 4 =>
							build_submodules_tree (
								module_name 	=> module,
								log_threshold	=> log_threshold + 1
								);

						when 5 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_CHECK =>
			case noun is
				when NOUN_INTEGRITY =>
					case fields is
						when 4 =>
							check_integrity (
								module_name 	=> module,
								log_threshold	=> log_threshold + 1);

						when 5 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
						
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_COPY =>
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 9 =>
							copy_device (
								module_name 	=> module,
								device_name		=> to_name (f (5)),
								destination		=> to_position 
									(
									sheet => to_sheet (f (6)),
									point => type_point (set
												(
												x => to_distance (f (7)),
												y => to_distance (f (8))
												)),
									rotation		=> to_rotation (f (9))
									),
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_SUBMODULE =>
					case fields is
						when 9 =>
							copy_submodule (
								module_name 	=> module, -- parent module (where the submodule is to be copied)
								instance_origin	=> et_general.to_instance_name (f (5)), -- submodule instance name
								instance_new	=> et_general.to_instance_name (f (6)), -- submodule instance name
								destination		=> to_position 
									(
									sheet => to_sheet (f (7)),
									point => type_point (set
												(
												x => to_distance (f (8)),
												y => to_distance (f (9))
												))
									),
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_CREATE =>
			case noun is
				when NOUN_VARIANT => 
					case fields is
						when 5 =>
							create_assembly_variant
								(
								module_name		=> module,
								variant_name	=> to_variant (f (5)),
								log_threshold	=> log_threshold + 1);
							
						when 6 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;
																
		when VERB_DELETE =>
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 5 =>
							delete_device (
								module_name 	=> module,
								device_name		=> to_name (f (5)),
								log_threshold	=> log_threshold + 1);

						when 6 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_LABEL =>
					case fields is
						when 7 =>
							delete_net_label
								(
								module_name		=> module,

								position		=> to_position (
													point => type_point (set (
														x => to_distance (f (6)),
														y => to_distance (f (7)))),
													sheet => to_sheet (f (5))), -- sheet number
								
								log_threshold	=> log_threshold + 1);
							
						when 8 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_NET =>
					case fields is

						-- If the statement has only 6 fields, the net scope is EVERYWHERE.
						-- Place assumes default (sheet 1, x/y 0/0) and is further-on ignored 
						-- by the called procedure:
						when 5 =>
							delete_net
								(
								module_name			=> module,
								net_name			=> to_net_name (f (5)), -- RESET
								scope				=> EVERYWHERE,
								place				=> to_position (
														point => origin,
														sheet => 1),
								log_threshold		=> log_threshold + 1);

						-- If the statement has 7 fields, the net scope is SHEET.
						-- Sheet is set by the 7th argument. x and y assume default (0/0)
						-- and are further-on ignored by the called procedure:
						when 6 =>
							delete_net
								(
								module_name			=> module,
								net_name			=> to_net_name (f (5)), -- RESET
								scope				=> SHEET,
								place				=> to_position (
														point => origin,
														sheet => to_sheet (f (6))), -- sheet number
								log_threshold		=> log_threshold + 1);

						-- If the statement has 9 fields, the net scope is STRAND.
						-- Place is set according to arguments 7..9.
						when 8 =>
							delete_net
								(
								module_name			=> module,
								net_name			=> to_net_name (f (5)), -- RESET
								scope				=> STRAND,
								place				=> to_position (
														point => type_point (set (
															x => to_distance (f (7)),
															y => to_distance (f (8)))),
														sheet => to_sheet (f (6))), -- sheet number
								log_threshold		=> log_threshold + 1);

							
						when 9 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);

					end case;

				when NOUN_NETCHANGER =>
					case fields is
						when 5 =>
							delete_netchanger
								(
								module_name		=> module,
								index			=> et_submodules.to_netchanger_id (f (5)), -- 1,2,3,...
								log_threshold		=> log_threshold + 1);

						when 6 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_PORT =>
					case fields is
						when 6 =>
							delete_port
								(
								module_name 	=> module,
								instance		=> et_general.to_instance_name (f (5)),
								port_name		=> et_general.to_net_name (f (6)),
								log_threshold	=> log_threshold + 1
								);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_SEGMENT =>
					case fields is
						when 8 =>
							delete_segment
								(
								module_name		=> module,
								net_name		=> to_net_name (f (5)), -- RESET
								place			=> to_position (
													point => type_point (set (
														x => to_distance (f (7)),
														y => to_distance (f (8)))),
													sheet => to_sheet (f (6))), -- sheet number
								log_threshold	=> log_threshold + 1);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_SUBMODULE =>
					case fields is
						when 5 =>
							delete_submodule (
								module_name 	=> module, -- parent module (where the submodule is to be deleted)
								instance		=> et_general.to_instance_name (f (5)), -- submodule instance name
								log_threshold	=> log_threshold + 1
								);

						when 6 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_TEXT =>
					NULL; -- CS
					
				when NOUN_UNIT =>
					case fields is
						when 6 =>
							delete_unit (
								module_name 	=> module,
								device_name		=> to_name (f (5)),
								unit_name		=> to_name (f (6)),
								log_threshold	=> log_threshold + 1);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_VARIANT => 
					case fields is
						when 5 =>
							delete_assembly_variant
								(
								module_name		=> module,
								variant_name	=> to_variant (f (5)),
								log_threshold	=> log_threshold + 1);
							
						when 6 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_DESCRIBE =>
			case noun is
				when NOUN_VARIANT => 
					case fields is
						when 6 =>
							describe_assembly_variant
								(
								module_name		=> module,
								variant_name	=> to_variant (f (5)), -- low_cost
								description		=> et_assembly_variants.to_unbounded_string (f (6)), -- "the cheap version"
								log_threshold	=> log_threshold + 1);
							
						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_DISPLAY => -- GUI related
			case noun is
				when NOUN_PORTS		-- like "schematic led_driver display ports [on/off]"
					| NOUN_NETS		-- like "schematic led_driver display nets [on/off]"
					| NOUN_NAMES | NOUN_VALUES | NOUN_PURPOSES
					| NOUN_TEXTS | NOUN_GRID
					=>
					case fields is
						when 4 => display (noun); -- if status is omitted
						when 5 => display (noun, f (5));
						when 6 .. count_type'last => too_long; 
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
					
		when VERB_DRAG =>
			case noun is
				when NOUN_UNIT =>
					case fields is
						when 9 =>
							drag_unit
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)),
								unit_name		=> to_name (f (6)),
								coordinates		=> to_coordinates (f (7)), -- relative/absolute
								point			=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_NETCHANGER =>
					case fields is
						when 8 =>
							drag_netchanger (
								module_name 	=> module,
								index			=> et_submodules.to_netchanger_id (f (5)), -- 1,2,3,...
								coordinates		=> to_coordinates (f (6)), -- relative/absolute
								point			=> type_point (set (
													x => to_distance (f (7)),
													y => to_distance (f (8)))),
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_PORT =>
					case fields is
						when 9 =>
							drag_port (
								module_name 	=> module,
								instance		=> et_general.to_instance_name (f (5)),
								port_name		=> et_general.to_net_name (f (6)),
								coordinates		=> to_coordinates (f (7)),  -- relative/absolute
								point			=> type_point (set (
											x => to_distance (f (8)),
											y => to_distance (f (9)))),
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_SEGMENT =>
					case fields is
						when 11 =>
							drag_segment
								(
								module_name		=> module,
								net_name		=> to_net_name (f (5)), -- RESET
								place			=> to_position (
													point => type_point (set (
														x => to_distance (f (7)),
														y => to_distance (f (8)))),
													sheet => to_sheet (f (6))), -- sheet number
								
								coordinates		=> to_coordinates (f (9)), -- relative/absolute
								
								point			=> type_point (set (
													x => to_distance (f (10)),
													y => to_distance (f (11)))),
								
								log_threshold	=> log_threshold + 1);

						when 12 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_SUBMODULE =>
					case fields is
						when 8 =>
							drag_submodule (
								module_name 	=> module,
								instance		=> et_general.to_instance_name (f (5)),
								coordinates		=> to_coordinates (f (6)),  -- relative/absolute
								point			=> type_point (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_DRAW =>
			case noun is
				when NOUN_NET =>
					case fields is
						when 10 =>
							draw_net
								(
								module_name		=> module,
								net_name		=> to_net_name (f (5)), -- RESET
								start_point		=> to_position (
														point => type_point (set (
															x => to_distance (f (7)),
															y => to_distance (f (8)))),
														sheet => to_sheet (f (6))), -- sheet number
								
								end_point		=> type_point (set (
													x => to_distance (f (9)),
													y => to_distance (f (10)))),
								
								log_threshold	=> log_threshold + 1);

						when 11 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
					
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
			
		when VERB_INVOKE =>
			case noun is
				when NOUN_UNIT =>
					case fields is
						when 10 =>
							invoke_unit (
								module_name		=> module,
								device_name		=> to_name (f (5)),
								unit_name		=> to_name (f (6)),
								destination		=> to_position 
									(
									sheet => to_sheet (f (7)),
									point => type_point (set
												(
												x => to_distance (f (8)),
												y => to_distance (f (9))
												)),
									rotation		=> to_rotation (f (10))
									),
								log_threshold	=> log_threshold + 1
								);

						when 11 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_MOVE =>
			case noun is
				when NOUN_NAME =>
					case fields is
						when 9 =>
							move_unit_placeholder
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								unit_name		=> to_name (f (6)), -- A
								coordinates		=> to_coordinates (f (7)),  -- relative/absolute
								point			=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
								meaning			=> et_symbols.NAME,
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_VALUE =>
					case fields is
						when 9 =>
							move_unit_placeholder
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								unit_name		=> to_name (f (6)), -- A
								coordinates		=> to_coordinates (f (7)),  -- relative/absolute
								point			=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
								meaning			=> et_symbols.VALUE,
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_PORT =>
					case fields is
						when 9 =>
							move_port (
								module_name 	=> module,
								instance		=> et_general.to_instance_name (f (5)),
								port_name		=> et_general.to_net_name (f (6)),
								coordinates		=> to_coordinates (f (7)),  -- relative/absolute
								point			=> type_point (set (
											x => to_distance (f (8)),
											y => to_distance (f (9)))),
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_PURPOSE =>
					case fields is
						when 9 =>
							move_unit_placeholder
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								unit_name		=> to_name (f (6)), -- A
								coordinates		=> to_coordinates (f (7)),  -- relative/absolute
								point			=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
								meaning			=> et_symbols.PURPOSE,
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_NETCHANGER =>
					case fields is
						when 9 =>
							move_netchanger
								(
								module_name 	=> module,
								index			=> et_submodules.to_netchanger_id (f (5)), -- 1,2,3, ...
								coordinates		=> to_coordinates (f (6)),  -- relative/absolute
								sheet			=> to_sheet_relative (f (7)),
								point			=> type_point (set (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
									
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_TEXT =>
					NULL; -- CS

				when NOUN_SUBMODULE =>
					case fields is
						when 9 =>
							move_submodule (
								module_name 	=> module,
								instance		=> et_general.to_instance_name (f (5)),
								coordinates		=> to_coordinates (f (6)),  -- relative/absolute
								sheet			=> to_sheet_relative (f (7)),
								point			=> type_point (set (
											x => to_distance (f (8)),
											y => to_distance (f (9)))),
								log_threshold	=> log_threshold + 1
								);

						when 10 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_UNIT =>
					case fields is
						when 10 =>
							move_unit
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								unit_name		=> to_name (f (6)), -- A
								coordinates		=> to_coordinates (f (7)),  -- relative/absolute
								sheet			=> to_sheet_relative (f (8)),
								point			=> type_point (set (
													x => to_distance (f (9)),
													y => to_distance (f (10)))),
									
								log_threshold	=> log_threshold + 1
								);

						when 11 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_MAKE =>
			case noun is
				when NOUN_BOM => 
					case fields is
						when 4 =>
							make_boms -- a BOM for each variant
								(
								module_name 	=> module,
								log_threshold	=> log_threshold + 1);

						when 5 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_NETLISTS => 
					case fields is
						when 4 =>
							make_netlists 
								(
								module_name 	=> module,
								log_threshold	=> log_threshold + 1);

						when 5 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_MOUNT =>
			case noun is
				when NOUN_DEVICE => 
					declare
						value : type_value.bounded_string; -- 470R
						partcode : et_material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
						purpose : type_purpose.bounded_string; -- brightness_control
					begin
						-- validate value
						value := to_value (f (7));

						-- validate partcode
						partcode := et_material.to_partcode (f (8));
						
						case fields is
							when 8 =>
								-- set value and partcode
								mount_device
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)), -- low_cost
									device			=> to_name (f (6)), -- R1
									value			=> value, -- 220R
									partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
									log_threshold	=> log_threshold + 1);

							when 9 =>
								-- optionally the purpose can be set also
								purpose := to_purpose (f (9)); -- brightness_control
											
								mount_device
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)), -- low_cost
									device			=> to_name (f (6)), -- R1
									value			=> value, -- 220R
									partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
									purpose			=> purpose, -- brightness_control
									log_threshold	=> log_threshold + 1);
								
							when 10 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					end; -- declare

				when NOUN_SUBMODULE =>
					case fields is
						when 7 =>
							mount_submodule
								(
								module_name		=> module,
								variant_parent	=> to_variant (f (5)), -- low_cost
								instance		=> et_general.to_instance_name (f (6)), -- OSC1
								variant_submod	=> to_variant (f (7)), -- fixed_frequency
								log_threshold	=> log_threshold + 1);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);

					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_PLACE =>
			case noun is
				when NOUN_JUNCTION =>
					case fields is
						when 7 =>
							place_junction 
								(
								module_name 	=> module,
								place			=> to_position 
													(
													sheet => to_sheet (f (5)),
													point => type_point (set (
																x => to_distance (f (6)),
																y => to_distance (f (7))
																))
													),
									
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_LABEL =>
					case fields is
						when 10 =>
							-- SIMPLE LABEL
							place_net_label
								(
								module_name			=> module,

								segment_position	=> to_position (
														point => type_point (set (
															x => to_distance (f (6)),
															y => to_distance (f (7)))),
														sheet => to_sheet (f (5))), -- sheet number

								label_position		=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),

								rotation			=> to_rotation (f (10)), -- 0 / 90
								appearance 			=> et_schematic.SIMPLE,

								-- A simple label does not indicate the direction
								-- of information flow. But this procedure call requires a
								-- direction. So we just pass direction PASSIVE. It has no 
								-- further meaning.
								direction			=> et_schematic.PASSIVE,

								log_threshold		=> log_threshold + 1);

						when 8 =>
							-- TAG LABEL
							place_net_label
								(
								module_name			=> module,

								segment_position	=> to_position (
														point => type_point (set (
															x => to_distance (f (6)),
															y => to_distance (f (7)))),
														sheet => to_sheet (f (5))), -- sheet number

								appearance 			=> et_schematic.TAG,

								-- A tag label requires specification of signal direction:
								direction			=> et_schematic.to_direction (f (8)), -- INPUT, OUTPUT, PASSIVE, ...

								log_threshold		=> log_threshold + 1);
							
						when 11 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd); -- incl. field count of 9
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
			
		when VERB_REMOVE =>
			case noun is
				when NOUN_DEVICE => 
					case fields is
						when 6 =>
							remove_device -- from assembly variant
								(
								module_name		=> module,
								variant_name	=> to_variant (f (5)), -- low_cost
								device			=> to_name (f (6)), -- R1
								log_threshold	=> log_threshold + 1);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when NOUN_SUBMODULE =>
					case fields is
						when 6 =>
							remove_submodule
								(
								module_name		=> module,
								variant_parent	=> to_variant (f (5)),
								instance		=> et_general.to_instance_name (f (6)), -- OSC1
								log_threshold	=> log_threshold + 1);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_RENAME =>
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 6 =>
							rename_device
								(
								module_name 		=> module,
								device_name_before	=> to_name (f (5)), -- IC1
								device_name_after	=> to_name (f (6)), -- IC23
								log_threshold		=> log_threshold + 1
								);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case; 
							
				when NOUN_SUBMODULE =>
					case fields is
						when 6 =>
							rename_submodule
								(
								module_name		=> module,
								instance_old	=> et_general.to_instance_name (f (5)), -- OSC1
								instance_new	=> et_general.to_instance_name (f (6)), -- OSC2
								log_threshold	=> log_threshold + 1);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_NET =>
					case fields is

						-- If the statement has only 6 fields, the net scope is EVERYWHERE.
						-- Place assumes default (sheet 1, x/y 0/0) and is further-on ignored 
						-- by the called procedure:
						when 6 =>
							rename_net
								(
								module_name			=> module,
								net_name_before		=> to_net_name (f (5)), -- RESET
								net_name_after		=> to_net_name (f (6)), -- RESET_N
								scope				=> EVERYWHERE,
								place				=> to_position (
														point => origin,
														sheet => 1),
								log_threshold		=> log_threshold + 1);

						-- If the statement has 7 fields, the net scope is SHEET.
						-- Sheet is set by the 7th argument. x and y assume default (0/0)
						-- and are further-on ignored by the called procedure:
						when 7 =>
							rename_net
								(
								module_name			=> module,
								net_name_before		=> to_net_name (f (5)), -- RESET
								net_name_after		=> to_net_name (f (6)), -- RESET_N
								scope				=> SHEET,
								place				=> to_position (
														point => origin,
														sheet => to_sheet (f (7))), -- sheet number
								log_threshold		=> log_threshold + 1);

						-- If the statement has 9 fields, the net scope is STRAND.
						-- Place is set according to arguments 7..9.
						when 9 =>
							rename_net
								(
								module_name			=> module,
								net_name_before		=> to_net_name (f (5)), -- RESET
								net_name_after		=> to_net_name (f (6)), -- RESET_N
								scope				=> STRAND,
								place				=> to_position (
														point => type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
														sheet => to_sheet (f (7))), -- sheet number
								log_threshold		=> log_threshold + 1);

							
						when 10 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_RENUMBER =>
			case noun is
				when NOUN_DEVICES =>
					case fields is
						when 5 =>
							renumber_devices
								(
								module_name 	=> module,
								step_width		=> to_index (f (5)), -- 100
								log_threshold	=> log_threshold + 1
								);

						when 6 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_ROTATE =>
			case noun is
				when NOUN_TEXT =>
					NULL; -- CS

				when NOUN_UNIT =>
					case fields is
						when 8 =>
							rotate_unit
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								unit_name		=> to_name (f (6)), -- A
								coordinates		=> to_coordinates (f (7)),  -- relative/absolute
								rotation		=> to_rotation (f (8)), -- 90
								log_threshold	=> log_threshold + 1
								);

						when 9 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_NAME =>
					case fields is 
						when 7 =>
							rotate_unit_placeholder
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								unit_name		=> to_name (f (6)), -- A
								rotation		=> et_schematic.pac_text.to_rotation_doc (f (7)), -- 90
								meaning			=> et_symbols.NAME,
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_VALUE =>
					case fields is
						when 7 =>
							rotate_unit_placeholder
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								unit_name		=> to_name (f (6)), -- A
								rotation		=> et_schematic.pac_text.to_rotation_doc (f (7)), -- 90
								meaning			=> et_symbols.VALUE,
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_PURPOSE =>
					case fields is
						when 7 =>
							rotate_unit_placeholder
								(
								module_name 	=> module,
								device_name		=> to_name (f (5)), -- IC1
								unit_name		=> to_name (f (6)), -- A
								rotation		=> et_schematic.pac_text.to_rotation_doc (f (7)), -- 90
								meaning			=> et_symbols.PURPOSE,
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_NETCHANGER =>
					case fields is
						when 7 =>
							rotate_netchanger (
								module_name 	=> module,
								index			=> et_submodules.to_netchanger_id (f (5)), -- 1,2,3,...
								coordinates		=> to_coordinates (f (6)), -- relative/absolute
								rotation		=> to_rotation (f (7)), -- 90
								log_threshold	=> log_threshold + 1
								);

						when 8 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_SET =>
			case noun is
				when NOUN_GRID =>
					case fields is
						-- schematic led_driver set grid 5 5
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
			
				when NOUN_PARTCODE =>
					case fields is
						when 6 =>
							declare
								partcode : et_material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
							begin
								partcode := et_material.to_partcode (f (6));

								-- set the purpose
								set_partcode
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- R1
									partcode		=> partcode, -- R_PAC_S_0805_VAL_100R
									log_threshold	=> log_threshold + 1
									);
							end;

						when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_PURPOSE =>
					case fields is
						when 6 =>
							declare
								use et_schematic;
								purpose : type_purpose.bounded_string; -- brightness_control
							begin
								purpose := to_purpose (f (6));
								
								-- set the purpose
								set_purpose
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- R1
									purpose			=> purpose, -- brightness_control
									log_threshold	=> log_threshold + 1
									);
							end;

						when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_SCOPE =>
					case fields is
						when 6 =>
							set_scope (
								module_name 	=> module,
								net_name		=> et_general.to_net_name (f (5)),
								scope			=> et_netlists.to_net_scope (f (6)),
								log_threshold	=> log_threshold + 1
								);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_SUBMODULE_FILE =>
					case fields is
						when 6 =>
							set_submodule_file (
								module_name 	=> module,
								instance		=> et_general.to_instance_name (f (5)),
								file			=> et_submodules.to_submodule_path (f (6)),
								log_threshold	=> log_threshold + 1
								);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_VALUE =>
					case fields is
						when 6 =>
							declare
								value : type_value.bounded_string; -- 470R
							begin
								-- validate value
								value := to_value (f (6));

								-- set the value
								set_value
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- R1
									value			=> value, -- 470R
									log_threshold	=> log_threshold + 1
									);
							end;

						when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
							
						when others => command_incomplete (cmd);
					end case;
							
				when NOUN_TEXT_SIZE =>
					NULL; -- CS
					
				when others => invalid_noun (to_string (noun));
			end case;

		when VERB_SHOW => -- GUI related
			case noun is
				when NOUN_DEVICE =>
					case fields is
						when 5 => show_first_unit; -- show device R1
						when 6 => show_unit; -- show device IC45 C
						when 7 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;

				when NOUN_MODULE =>
					case fields is
						when 5 => show_module; -- show module LED-driver
						when 6 => show_module_and_sheet; -- show module LED-driver 2
						when 7 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;
					
				when NOUN_SHEET =>
					case fields is
						when 5 => show_sheet;
						when 6 .. count_type'last => too_long;
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_UNMOUNT =>
			case noun is
				when NOUN_DEVICE => 
					case fields is
						when 6 =>
							unmount_device
								(
								module_name		=> module,
								variant_name	=> to_variant (f (5)), -- low_cost
								device			=> to_name (f (6)), -- R1
								log_threshold	=> log_threshold + 1);

						when 7 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when others => invalid_noun (to_string (noun));
			end case;
			
		when VERB_WRITE =>
			case noun is
				when NOUN_TEXT =>
					NULL; -- CS

				when others => invalid_noun (to_string (noun));
			end case;

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
						when 5 => 
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

	canvas.update_drawing_mode_display;
	
	return exit_code;

	
	exception when event: others => 

		log (text => "mode " & to_string (op_mode), level => log_threshold, console => true);

		canvas.update_drawing_mode_display;
		
		log (ERROR, "schematic command " & enclose_in_quotes (to_string (cmd)) &
			" invalid !", console => true);

		log (text => ada.exceptions.exception_information (event), console => true);		

	return ERROR;

end schematic_cmd;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

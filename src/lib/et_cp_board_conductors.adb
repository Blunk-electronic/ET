------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  COMMAND PROCESSOR / BOARD / CONDUCTORS                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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
-- To Do:
-- - rework
-- - propose arguments if command incomplete
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_runmode;						use et_runmode;
with et_primitive_objects;				use et_primitive_objects;
with et_exceptions;						use et_exceptions;
with et_modes.board;					use et_modes.board;
with et_module;							use et_module;
with et_module_names;					use et_module_names;
with et_fill_zones;						use et_fill_zones;
with et_fill_zones.boards;				use et_fill_zones.boards;
with et_thermal_relief;					use et_thermal_relief;
with et_terminals;
with et_keywords;						use et_keywords;

with et_module_board_user_settings;
with et_board_geometry;					use et_board_geometry;
with et_keywords;						use et_keywords;

with et_ratsnest;
with et_board_ops.ratsnest;
with et_board_ops_user_settings;		use et_board_ops_user_settings;
with et_board_ops_fill_zones;
with et_board_ops_conductors;

with et_pcb_signal_layers;
with et_net_names;						use et_net_names;
with et_device_name;
with et_terminal_name;
with et_axes;
with et_directions;

with et_canvas_board;




package body et_cp_board_conductors is

	use pac_geometry_2;
	use pac_contours;
	use pac_grid;
	use pac_generic_modules;

	

	procedure set_fill_zone_properties (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);
		

		
		procedure expect_keywords is 
			comma : constant character := ',';
		begin
			raise syntax_error_1 with 
				"ERROR: Expect keyword "
				& enclose_in_quotes (keyword_linewidth) & comma
				& enclose_in_quotes (keyword_connection) & comma
				& enclose_in_quotes (keyword_priority) & comma
				& enclose_in_quotes (keyword_spacing) & comma
				& enclose_in_quotes (keyword_relief) & comma
				& enclose_in_quotes (keyword_easing) & comma & " or "
				& enclose_in_quotes (keyword_isolation)
				& " after keyword " & enclose_in_quotes (to_lower (to_string (noun))) & " !";
		end expect_keywords;


		procedure set_fill_style (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.fill_style := to_fill_style (get_field (cmd, 6));
		end set_fill_style;

		
		procedure set_linewidth (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.linewidth := to_distance (get_field (cmd, 6));
		end set_linewidth;

		
		procedure set_iso (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.isolation := to_distance (get_field (cmd, 6));
		end set_iso;

		
		procedure set_priority (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use et_fill_zones.boards;
		begin
			module.board.user_settings.polygons_conductor.priority_level := to_priority (get_field (cmd, 6));
		end set_priority;

		
		procedure set_easing_style (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.easing.style := to_easing_style (get_field (cmd, 7));
		end set_easing_style;

		
		procedure set_easing_radius (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.easing.radius := to_distance (get_field (cmd, 7));
		end set_easing_radius;	

		
		procedure set_connection (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.connection := to_pad_connection (get_field (cmd, 6));
		end set_connection;	

		
		procedure set_hatching_spacing (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.spacing := to_distance (get_field (cmd, 6));
		end set_hatching_spacing;	

		
		procedure set_thermal_width (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.thermal.width_min := to_distance (get_field (cmd, 7));
		end set_thermal_width;	

		
		procedure set_thermal_gap (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.polygons_conductor.thermal.gap_max := to_distance (get_field (cmd, 7));
		end set_thermal_gap;	

		
	begin -- set_fill_zone_properties
		-- CS log message

		
		case cmd_field_count is
			when 6 => 
				-- board demo set zone fill solid/hatched
				if get_field (cmd, 5) = keyword_fill then
					update_element (generic_modules, module, set_fill_style'access);

				-- board demo set zone linewidth 0.25
				elsif get_field (cmd, 5) = keyword_linewidth then
					update_element (generic_modules, module, set_linewidth'access);

				-- board demo set zone spacing 0.3
				elsif get_field (cmd, 5) = keyword_spacing then
					update_element (generic_modules, module, set_hatching_spacing'access);
					
				-- board demo set zone isolaton 0.4
				elsif get_field (cmd, 5) = keyword_isolation then
					update_element (generic_modules, module, set_iso'access);
					
				-- board demo set zone priority 2
				elsif get_field (cmd, 5) = keyword_priority then
					update_element (generic_modules, module, set_priority'access);

				-- board demo set zone connection thermal/solid
				elsif get_field (cmd, 5) = keyword_connection then
					update_element (generic_modules, module, set_connection'access);

				else
					expect_keywords;
				end if;

				
			when 7 =>
				-- board demo set zone easing style none/chamfer/fillet
				if get_field (cmd, 5) = keyword_easing then

					if get_field (cmd, 6) = keyword_style then
						update_element (generic_modules, module, set_easing_style'access);

					elsif get_field (cmd, 6) = keyword_radius then
						update_element (generic_modules, module, set_easing_radius'access);

					else
						raise syntax_error_1 with
							"ERROR: Expect keywords " 
							& enclose_in_quotes (keyword_style) & " or "
							& enclose_in_quotes (keyword_radius) 
							& " after keyword " & enclose_in_quotes (keyword_easing) & " !";
					end if;


				-- board demo set zone relief width_min/ gap_max 0.3
				elsif get_field (cmd, 5) = keyword_relief then

					if get_field (cmd, 6) = keyword_width_min then
						update_element (generic_modules, module, set_thermal_width'access);

					elsif get_field (cmd, 6) = keyword_gap_max then
						update_element (generic_modules, module, set_thermal_gap'access);

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
				
			when 8 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);

			when others => command_incomplete (cmd);
				
		end case;
	end set_fill_zone_properties;


	



	type type_track_shape is (LINE, ARC, ZONE);
	-- CS circular tracks are currently not supported

		
	


	procedure route_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		use et_pcb_signal_layers;
		use et_terminals;
		use et_terminal_name;
		use et_board_ops;
		use et_board_ops_conductors;
		use et_net_names;
		use et_device_name;
		use et_axes;
		use et_directions;
		
		
		shape : constant type_track_shape := type_track_shape'value (get_field (cmd, 7));

		-- get the user specific settings of the board
		settings : constant et_module_board_user_settings.type_user_settings := 
			get_user_settings (module);


		
		procedure make_fill_zone is
			use et_board_ops_fill_zones;
			
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
						layer 			=> to_signal_layer (get_field (cmd, 6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						others			=> <>),
					others				=> <>);
					
				add_zone (
					module_cursor	=> module,
					zone			=> p2,
					net_name		=> to_net_name (get_field (cmd, 5)),
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
						layer 			=> to_signal_layer (get_field (cmd, 6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						others			=> <>),
					others				=> <>);
					
				add_zone (
					module_cursor	=> module,
					zone			=> p2,
					net_name		=> to_net_name (get_field (cmd, 5)),
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
						layer 			=> to_signal_layer (get_field (cmd, 6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						others			=> <>),
					others				=> <>);
					
				add_zone (
					module_cursor	=> module,
					zone			=> p2,
					net_name		=> to_net_name (get_field (cmd, 5)),
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
						layer 			=> to_signal_layer (get_field (cmd, 6)),
						priority_level	=> settings.polygons_conductor.priority_level,
						others			=> <>),
					others				=> <>);
					
				add_zone (
					module_cursor	=> module,
					zone			=> p2,
					net_name		=> to_net_name (get_field (cmd, 5)),
					log_threshold	=> log_threshold + 1);
				
			end make_hatched_solid;

			
		begin
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



		line_tmp	: type_line;
		arc_tmp		: type_arc;
		width_tmp	: type_distance_positive;
		layer_tmp	: type_signal_layer;

		
	begin -- route_net
		-- CS log message
		
		case shape is
			when LINE =>
				if is_number (get_field (cmd, 9)) then -- 33.4 or IC4
					
					-- THE TRACK STARTS AT A DEDICATED POINT AT X/Y:
					
					-- board motor_driver route net NET_1 2 line 0.25 0 0 160 0
					case cmd_field_count is
						when 12 =>
							layer_tmp := to_signal_layer (get_field (cmd, 6));
							width_tmp := to_distance (get_field (cmd, 8));
							
							line_tmp := type_line (to_line (
								A => to_vector_model (get_field (cmd, 9), get_field (cmd, 10)),
								B => to_vector_model (get_field (cmd, 11), get_field (cmd, 12))));
														
							add_line (
								module_name 	=> key (module),
								net_name		=> to_net_name (get_field (cmd, 5)),
								line			=> (line_tmp with width_tmp, layer_tmp),								
								log_threshold	=> log_threshold + 1);

						when 13 .. type_field_count'last =>
							command_too_long (cmd, cmd_field_count - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				else
					-- THE TRACK STARTS AT A TERMINAL:
					
					if get_field (cmd, 11) = keyword_to then
						-- board motor_driver route net NET_1 1 line 0.25 R1 1 to 35 40
						-- board motor_driver route net NET_1 1 line 0.25 R1 1 to x 5
						
						if is_number (get_field (cmd, 12)) then
							-- THE TRACK ENDS AT A DEDICATED POINT X/Y
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 to 35 40
							case cmd_field_count is
								when 13 =>
									add_line (
										module_name 	=> key (module),
										net_name		=> to_net_name (get_field (cmd, 5)),
										layer			=> to_signal_layer (get_field (cmd, 6)),
										width			=> to_distance (get_field (cmd, 8)),
										device			=> to_device_name (get_field (cmd, 9)),
										terminal		=> to_terminal_name (get_field (cmd, 10)),
										end_point		=> to_vector_model (get_field (cmd, 12), get_field (cmd, 13)), -- 35 40
										log_threshold	=> log_threshold + 1);
									
								when 14 .. type_field_count'last =>
									command_too_long (cmd, cmd_field_count - 1);
									
								when others =>
									command_incomplete (cmd);
							end case;
									
						else
							-- THE TRACK ENDS ON A GRID LINE ALONG A GIVEN AXIS:
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 to x 5
							if get_field (cmd, 12) = to_string (AXIS_X) or get_field (cmd, 12) = to_string (AXIS_Y) then
								case cmd_field_count is
									when 13 =>
										add_line (
											module_name => key (module),
											net_name	=> to_net_name (get_field (cmd, 5)),
											layer		=> to_signal_layer (get_field (cmd, 6)),
											width		=> to_distance (get_field (cmd, 8)),
											device		=> to_device_name (get_field (cmd, 9)),
											terminal	=> to_terminal_name (get_field (cmd, 10)),
											axis		=> to_axis (get_field (cmd, 12)),
											notches		=> to_notches (get_field (cmd, 13)), -- 5
											
											log_threshold	=> log_threshold + 1
											);
										
									when 14 .. type_field_count'last =>
										command_too_long (cmd, cmd_field_count - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;
								
							else
								invalid_keyword (12);
							end if;
						end if;
						
						
					elsif get_field (cmd, 11) = keyword_direction then
						-- THE TRACK RUNS INTO GIVEN DIRECTION SPECIFIED BY AN ANGLE
						
						if is_number (get_field (cmd, 13)) then
							-- THE TRACK ENDS AFTER A GIVEN DISTANCE (it has a given length)
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 direction 45 50
							
							case cmd_field_count is
								when 13 =>
									add_line (
										module_name => key (module),
										net_name	=> to_net_name (get_field (cmd, 5)),
										layer		=> to_signal_layer (get_field (cmd, 6)),
										width		=> to_distance (get_field (cmd, 8)),
										device		=> to_device_name (get_field (cmd, 9)),
										terminal	=> to_terminal_name (get_field (cmd, 10)),
										direction	=> to_rotation (get_field (cmd, 12)), -- 45 degree
										length		=> to_distance (get_field (cmd, 13)), -- 50mm
										
										log_threshold	=> log_threshold + 1
										);

								when 14 .. type_field_count'last =>
									command_too_long (cmd, cmd_field_count - 1);
									
								when others =>
									command_incomplete (cmd);
							end case;

						else
							-- THE TRACK ENDS AT A GIVEN GRID LINE ALONG A GIVEN AXIS
							
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 direction 45 x 5
							if get_field (cmd, 13) = to_string (AXIS_X) or get_field (cmd, 13) = to_string (AXIS_Y) then
								
								case cmd_field_count is
									when 14 =>
										add_line (
											module_name => key (module),
											net_name	=> to_net_name (get_field (cmd, 5)),
											layer		=> to_signal_layer (get_field (cmd, 6)),
											width		=> to_distance (get_field (cmd, 8)),
											device		=> to_device_name (get_field (cmd, 9)),
											terminal	=> to_terminal_name (get_field (cmd, 10)),
											direction	=> to_rotation (get_field (cmd, 12)), -- 45 degree
											axis		=> to_axis (get_field (cmd, 13)),
											notches		=> to_notches (get_field (cmd, 14)), -- 5
											
											log_threshold	=> log_threshold + 1
											);

									when 15 .. type_field_count'last =>
										command_too_long (cmd, cmd_field_count - 1);
										
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
				case cmd_field_count is
					when 15 =>
						arc_tmp := type_arc (to_arc (
							center		=> type_vector_model (set (
								x => to_distance (dd => get_field (cmd, 9)),
								y => to_distance (dd => get_field (cmd, 10)))),
							A	=> type_vector_model (set (
								x => to_distance (dd => get_field (cmd, 11)),
								y => to_distance (dd => get_field (cmd, 12)))),
							B	=> type_vector_model (set (
								x => to_distance (dd => get_field (cmd, 13)),
								y => to_distance (dd => get_field (cmd, 14)))),
							direction	=> to_direction (get_field (cmd, 15))));
														
						-- draw a named track
						add_arc (
							module_name => key (module),
							net_name	=> to_net_name (get_field (cmd, 5)),
							arc			=> (arc_tmp with
								layer	=> to_signal_layer (get_field (cmd, 6)),
								width	=> to_distance (get_field (cmd, 8))),
							log_threshold	=> log_threshold + 1
							);
						
					when 16 .. type_field_count'last => 
						command_too_long (cmd, cmd_field_count - 1);
						
					when others =>
						command_incomplete (cmd);
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
						command_incomplete (cmd);
				end case;
				
		end case;
	end route_net;






	


	procedure show_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);
	begin
		-- CS log message
		null;
	end show_net;





	



	procedure delete_net_segment (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is 
			use et_net_names;
			use et_pcb_signal_layers;
			use et_board_ops_conductors;
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (cmd, 7), get_field (cmd, 8)),
				radius	=> to_zone_radius (get_field (cmd, 9)));
		
			delete_track (
				module_name 	=> key (module),
				net_name		=> to_net_name (get_field (cmd, 5)),
				layer			=> to_signal_layer (get_field (cmd, 6)),
				catch_zone		=> catch_zone,				
				log_threshold	=> log_threshold + 1);

		end do_it;

		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 9 => do_it;
			when 10 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	
	end delete_net_segment;






	
	
	procedure route_freetrack (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		use et_board_ops;
		use et_board_ops_conductors;
		use et_pcb_signal_layers;
		use et_net_names;
		use et_directions;
		
		shape : constant type_track_shape := type_track_shape'value (get_field (cmd, 6));

		-- get the user specific settings of the board
		settings : constant et_module_board_user_settings.type_user_settings := 
			get_user_settings (module);
		

		-- Extract from the given command the zone 
		-- arguments (everything after "zone"):
		procedure make_fill_zone is
			use et_fill_zones;
			use et_fill_zones.boards;
			use et_board_ops_fill_zones;
			
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
							layer 			=> to_signal_layer (get_field (cmd, 5)),
							priority_level	=> settings.polygons_conductor.priority_level,
							others			=> <>),

						islands		=> no_islands,
						easing		=> settings.polygons_conductor.easing);

					add_zone (module, ps, log_threshold + 1);

					
				when HATCHED =>

					ph := (p with 
						fill_style	=> HATCHED,
						spacing		=> settings.polygons_conductor.spacing,
						linewidth	=> settings.polygons_conductor.linewidth,
						isolation	=> settings.polygons_conductor.isolation,
						properties	=> (
							layer 			=> to_signal_layer (get_field (cmd, 5)),
							priority_level	=> settings.polygons_conductor.priority_level,
							others			=> <>),

						islands		=> no_islands,
						easing		=> settings.polygons_conductor.easing);

					add_zone (module, ph, log_threshold + 1);
					
			end case;
		end make_fill_zone;


		line_tmp	: type_line;
		arc_tmp		: type_arc;
		width_tmp	: type_distance_positive;
		layer_tmp	: type_signal_layer;

		
	begin -- route_freetrack
		-- CS log message
		
		case shape is
			when LINE =>
				case cmd_field_count is
					when 11 =>
						-- draw a freetrack
						layer_tmp := to_signal_layer (get_field (cmd, 5));
						width_tmp := to_distance (get_field (cmd, 7));

						line_tmp := type_line (to_line (
							A => to_vector_model (get_field (cmd, 8), get_field (cmd, 9)),
							B => to_vector_model (get_field (cmd, 10), get_field (cmd, 11))));
														
						add_line (
							module_name 	=> key (module),
							net_name		=> to_net_name (""),
							line			=> (line_tmp with width_tmp, layer_tmp),							
							log_threshold	=> log_threshold + 1);

					when 12 .. type_field_count'last =>
						command_too_long (cmd, cmd_field_count - 1);
						
					when others =>
						command_incomplete (cmd);
				end case;

				
			when ARC =>
				case cmd_field_count is
					when 14 =>
						layer_tmp := to_signal_layer (get_field (cmd, 5));
						width_tmp := to_distance (get_field (cmd, 7));

						arc_tmp := type_arc (to_arc (
							center		=> to_vector_model (get_field (cmd, 8), get_field (cmd, 9)),
							A			=> to_vector_model (get_field (cmd, 10), get_field (cmd, 11)),
							B			=> to_vector_model (get_field (cmd, 12), get_field (cmd, 13)),
							direction	=> to_direction (get_field (cmd, 14))));
														
						-- draw a freetrack
						add_arc (
							module_name 	=> key (module),
							arc				=> (arc_tmp with width_tmp, layer_tmp),
							net_name		=> to_net_name (""),
							log_threshold	=> log_threshold + 1);
						
					when 15 .. type_field_count'last =>
						command_too_long (cmd, cmd_field_count - 1);
						
					when others =>
						command_incomplete (cmd);
				end case;

				
			when ZONE =>
				case cmd_field_count is
					when 5 .. type_field_count'last =>
						make_fill_zone;

					when others =>
						command_incomplete (cmd);
				end case;
		end case;
	end route_freetrack;


	




	procedure delete_freetrack_segment (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		procedure do_it is 
			use et_net_names;
			use et_pcb_signal_layers;
			use et_board_ops_conductors;
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (cmd, 6), get_field (cmd, 7)),
				radius	=> to_zone_radius (get_field (cmd, 8)));
				
			delete_track (
				module_name 	=> key (module),
				net_name		=> to_net_name (""),
				layer			=> to_signal_layer (get_field (cmd, 5)),
				catch_zone		=> catch_zone,
				log_threshold	=> log_threshold + 1);

		end do_it;

		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 8 => do_it;
			
			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_freetrack_segment;








	procedure fill_zones (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		use et_canvas_board.pac_canvas;
		use et_board_ops_fill_zones;
		nets : pac_net_names.list;

	begin
		-- CS log message
		
		case cmd_field_count is
			when 4 => -- fill all zones
				
				-- command: board demo fill zone
				fill_zones (module, log_threshold + 1);

				
			when others => 
				-- like: board demo fill zone GND P3V3 AGND

				-- collect the optional net names in list "nets":
				for place in 5 .. cmd_field_count loop
					nets.append (to_net_name (get_field (cmd, place)));
				end loop;

				fill_zones (module, log_threshold + 1, nets);
		end case;
				
		if runmode /= MODE_HEADLESS then
			set_status ("conductor zones filled");
		end if;
	end fill_zones;
		








	procedure clear_zones (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		use et_canvas_board.pac_canvas;
		use et_board_ops_fill_zones;
		nets : pac_net_names.list;
	begin
		-- CS log message
		
		case cmd_field_count is
			when 4 => -- clear all zones
				
				-- command: board demo clear zone
				clear_zones (module, log_threshold + 1);

				
			when others => 
				-- like: board demo clear zone GND P3V3 AGND

				-- collect the optional net names in list "nets":
				for place in 5 .. cmd_field_count loop
					nets.append (to_net_name (get_field (cmd, place)));
				end loop;

				clear_zones (module, log_threshold + 1, nets);
		end case;
					
		if runmode /= MODE_HEADLESS then
			set_status ("conductor zone(s) cleared");
		end if;
	end clear_zones;





	

	procedure update_ratsnest (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		use et_canvas_board.pac_canvas;
		use et_ratsnest;
		use et_board_ops.ratsnest;
	begin
		-- CS log message
		
		
		case cmd_field_count is
				
			when 4 => 
				update_ratsnest (module, log_threshold + 1);
				set_status (status_ratsnest_updated);
			
			when 5 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end update_ratsnest;

	
		
end et_cp_board_conductors;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

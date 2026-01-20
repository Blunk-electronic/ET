------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / SCHEMATIC / UNIT OF A DEVICE          --
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
with et_modes.schematic;
with et_sheets;							use et_sheets;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_geometry;				use et_schematic_geometry;

with et_device_name;					use et_device_name;
with et_devices_electrical;				use et_devices_electrical;
with et_devices_electrical.units;		use et_devices_electrical.units;
with et_schematic_ops.units;			use et_schematic_ops.units;
with et_schematic_ops_device;			use et_schematic_ops_device;
with et_units;							use et_units;
with et_unit_name;						use et_unit_name;
with et_device_property_level;
with et_canvas_schematic;

with et_device_placeholders;
with et_rotation_docu;


package body et_cp_schematic_unit is

	use pac_generic_modules;
	use pac_geometry_2;




	procedure show_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_device_property_level;
	
		-- The degree of how much information is to be inqured:
		properties_level : type_properties_level;

		-- In order to tell the command processor that an operation is meant to 
		-- apply to the current sheet, we use the UNIX-bash-like period character:
		here : constant string := ".";
		
		
		-- Selects the device so that a certain unit or all its units become
		-- highlighted in the canvas.
		-- Sets the sheet where the unit is.
		-- Pans the canvas so that the unit is in the center of the view.
		-- 1. If mode is SEARCH_MODE_FIRST_UNIT then the sheet where the first unit is
		--    will be shown in the center of the canvas. All units of the
		--    device will be selected and highlighted.
		--    The given unit name will be ignored.
		--    Example: "schematic led_driver show device R1"
		--
		-- 2. If mode is SEARCH_MODE_BY_UNIT_NAME then the sheet where the given unit is
		--    will be shown in the center of the canvas. Only that unit
		--    of the device will be selected and highlighted.
		--    Example: "schematic led_driver show device IC1 IO-BANK2"
		--
		-- 3. If mode is SEARCH_MODE_FIRST_UNIT_ON_CURRENT_SHEET then the first unit
		--    on the current sheet will be shown in the center of the canvas.
		--    All units of the device will be selected and highlighted.
		--    Example: "schematic led_driver show device IC1 ."
		--
		procedure do_it (
			device	: in type_device_name; -- IC45
			unit	: in pac_unit_name.bounded_string := to_unit_name (""); -- A, B, ..
			mode	: in type_device_search_mode := SEARCH_MODE_FIRST_UNIT)
		is
			
			-- This small function performs a unit query:
			function locate_unit (unit : in pac_unit_name.bounded_string) 
				return type_unit_query
			is begin
				return get_unit_position (
					module_cursor	=> module,
					device_name		=> device,
					unit_name		=> unit);				
			end;

			
			procedure device_not_found is begin
				log (WARNING, " Device " & to_string (device) & " not found !");
				-- CS output in status bar
			end;

			
			procedure unit_not_found is 
				use pac_unit_name;
			begin
				log (WARNING, " Device " & to_string (device) 
					& " unit " & to_string (unit) & " not found !");
				-- CS output in status bar
			end;

			

			-- If no unit was specified by the caller, then this
			-- procedure searches for the first unit of the given device:
			procedure show_first_unit is
				use et_canvas_schematic;
				use et_canvas_schematic.pac_canvas;
				unit_query : constant type_unit_query := locate_unit (to_unit_name (""));
				error : boolean := false;
			begin
				if unit_query.exists then
					-- Set the active sheet where the unit is:
					active_sheet := get_sheet (unit_query.position);
					update_sheet_number_display;
					
					-- Center on the first unit and leave the
					-- zoom factor as it is. If the runmode is
					-- headless, then nothing happens here:
					zoom_to (get_place (unit_query.position), S);

					-- Highlight all units:
					show_device (
						module_cursor	=> module, 
						device_name		=> device, 
						all_units		=> true,
						unit_name		=> unit_name_default,
						error			=> error,
						log_threshold	=> log_threshold + 2);
					
					-- Show some basic information in the staus bar:
					set_status (get_device_properties (
						module_cursor	=> module, 
						device_name		=> device, 
						level			=> DEVICE_PROPERTIES_LEVEL_1,
						error			=> error,
						log_threshold	=> log_threshold + 2));

					-- For property levels greater 1 we open
					-- the properties window in order to conveniently
					-- show a lot of information:
					case properties_level is
						when DEVICE_PROPERTIES_LEVEL_1 => null;

						when others =>
							
							pac_device_ops.show_properties_window (
								device	=> device,
								text	=> get_device_properties (
									module_cursor	=> module, 
									device_name		=> device, 
									linebreaks		=> true,
									level			=> properties_level,
									error			=> error,
									log_threshold	=> log_threshold + 2));
					end case;
					
				else
					device_not_found;
				end if;
			end show_first_unit;


			
			-- If a unit was specified by the caller, then this
			-- procedure searches for the given unit of the given device:
			procedure show_by_unit_name is
				use et_canvas_schematic;
				use et_canvas_schematic.pac_canvas;
				unit_query : constant type_unit_query := locate_unit (unit);
				error : boolean := false;
			begin
				if unit_query.exists then
					-- Set the active sheet where the unit is:
					active_sheet := get_sheet (unit_query.position);
					update_sheet_number_display;
					
					-- Center on the first unit and leave the
					-- zoom factor as it is. If the runmode is
					-- headless, then nothing happens here:
					zoom_to (get_place (unit_query.position), S);

					-- Highlight the given unit only:
					show_device (
						module_cursor	=> module, 
						device_name		=> device, 
						all_units		=> false, 
						unit_name		=> unit,
						error			=> error,
						log_threshold	=> log_threshold + 2);
					
					-- Show some basic information in the staus bar:
					set_status (get_device_properties (
						module_cursor	=> module, 
						device_name		=> device, 
						level			=> DEVICE_PROPERTIES_LEVEL_1,
						all_units		=> false,
						unit_name		=> unit,
						error			=> error,
						log_threshold	=> log_threshold + 2));

					-- For property levels greater 1 we open
					-- the properties window in order to conveniently
					-- show a lot of information:
					case properties_level is
						when DEVICE_PROPERTIES_LEVEL_1 => null;

						when others =>
							
							pac_device_ops.show_properties_window (
								device	=> device,
								text	=> get_device_properties (
									module_cursor	=> module, 
									device_name		=> device, 
									linebreaks		=> true,
									level			=> properties_level,
									all_units		=> false,
									unit_name		=> unit,
									error			=> error,
									log_threshold	=> log_threshold + 2));

					end case;

					
				else
					unit_not_found;
				end if;
			end show_by_unit_name;



			-- If instead of a unit a "." was specified by the caller
			-- then the first unit on the active sheet is searched for:
			procedure show_first_unit_on_active_sheet is
				use et_canvas_schematic;
				use et_canvas_schematic.pac_canvas;
				unit_query : constant type_unit_query := locate_unit (to_unit_name (""));
				error : boolean := false;
			begin
				if unit_query.exists then
					if get_sheet (unit_query.position) = active_sheet then
						
						-- Center on the first unit and leave the
						-- zoom factor as it is. If the runmode is
						-- headless, then nothing happens here:
						zoom_to (get_place (unit_query.position), S);

						-- Highlight all units:
						show_device (
							module_cursor	=> module, 
							device_name		=> device, 
							all_units		=> true, 
							unit_name		=> unit_name_default,
							error			=> error,
							log_threshold	=> log_threshold + 2);

						-- Show some basic information in the staus bar:
						set_status (get_device_properties (
							module_cursor	=> module, 
							device_name		=> device, 
							level			=> DEVICE_PROPERTIES_LEVEL_1,
							error			=> error,
							log_threshold	=> log_threshold + 2));

						-- For property levels greater 1 we open
						-- the properties window in order to conveniently
						-- show a lot of information:
						case properties_level is
							when DEVICE_PROPERTIES_LEVEL_1 => null;

							when others =>
								
								pac_device_ops.show_properties_window (
									device	=> device,
									text	=> get_device_properties (
										module_cursor	=> module, 
										device_name		=> device, 
										linebreaks		=> true,
										level			=> properties_level,
										error			=> error,
										log_threshold	=> log_threshold + 2));	   

						end case;
						
						
					else
						log (WARNING, " Device " & to_string (device) & " is not on this sheet !");
						-- CS output in status bar
					end if;

				else
					device_not_found;
				end if;
			end show_first_unit_on_active_sheet;
			
			
		begin
			case mode is
				when SEARCH_MODE_FIRST_UNIT =>
					show_first_unit;					
					
				when SEARCH_MODE_BY_UNIT_NAME =>
					show_by_unit_name;
					
				when SEARCH_MODE_FIRST_UNIT_ON_CURRENT_SHEET =>
					show_first_unit_on_active_sheet;
								
			end case;
		end do_it;


		
		procedure runmode_module is 
			error : boolean := false;
		begin
			case cmd_field_count is
				when 6 => 
					-- show device L1 R1
					properties_level := to_properties_level (get_field (cmd, 5), error); -- L1
					
					if not error then						
						do_it (
							device	=> to_device_name (get_field (cmd, 6)), -- R1, IC1
							mode	=> SEARCH_MODE_FIRST_UNIT);
					end if;
				
				when 7 =>
					properties_level := to_properties_level (get_field (cmd, 5), error); -- L1
					
					if not error then
						-- The 7th field may be a period, which means
						-- the unit is to be shown on the current active sheet.
						-- Otherwise the field provides an explicit
						-- unit name:
						if get_field (cmd, 7) = here then
							do_it ( -- show device L1 IC1 .
								device	=> to_device_name (get_field (cmd, 6)), -- IC1
								mode	=> SEARCH_MODE_FIRST_UNIT_ON_CURRENT_SHEET);
						else
							do_it ( -- show device L1 IC1 A
								device	=> to_device_name (get_field (cmd, 6)), -- IC1
								unit	=> to_unit_name (get_field (cmd, 7)), -- A
								mode	=> SEARCH_MODE_BY_UNIT_NAME);
						end if;
					end if;
					
				when 8 .. type_field_count'last => 
					command_too_long (cmd, cmd_field_count - 1);
					
				when others => command_incomplete (cmd);
			end case;
		end runmode_module;

		
	begin
	-- CS log message
	
		-- Show operations are only useful and possible in graphical
		-- runmode:
		case runmode is
			when MODE_MODULE =>
				runmode_module;

			when others =>
				skipped_in_this_runmode (log_threshold + 1);
					
		end case;				
	end show_device;




	
	
	
	
	
	procedure delete_unit (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	
		device_name : type_device_name;
		unit_name	: pac_unit_name.bounded_string;
	begin
		device_name := to_device_name (get_field (cmd, 5));
		unit_name	:= to_unit_name (get_field (cmd, 6));
		
		case cmd_field_count is
			when 6 =>
				delete_unit (
					module_cursor 	=> module,
					device_name		=> device_name,
					unit_name		=> unit_name,
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_unit;

	
	
	
	
	
	
	
	procedure drag_unit (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is 
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 9 =>
				drag_unit (
					module_cursor 	=> module,
					device_name		=> to_device_name (get_field (cmd, 5)),
					unit_name		=> to_unit_name (get_field (cmd, 6)),
					coordinates		=> to_coordinates (get_field (cmd, 7)), -- relative/absolute
					destination		=> type_vector_model (set (
										x => to_distance (get_field (cmd, 8)),
										y => to_distance (get_field (cmd, 9)))),
					log_threshold	=> log_threshold + 1);

			when 10 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end drag_unit;

	
	
	
	
	
	
	
	
	
	procedure move_unit (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		device_name : type_device_name;
		unit_name	: pac_unit_name.bounded_string;
		coordinates : type_coordinates;
		sheet		: type_sheet_relative;		
		destination	: type_vector_model;
	begin
		device_name := to_device_name (get_field (cmd, 5)); -- IC1
		unit_name	:= to_unit_name (get_field (cmd, 6)); -- A
		coordinates := to_coordinates (get_field (cmd, 7)); -- relative/absolute
		sheet		:= to_sheet_relative (get_field (cmd, 8)); -- -1, 2

		destination	:= set (x => to_distance (get_field (cmd, 9)), -- 2, 210
							y => to_distance (get_field (cmd, 10))); -- 4, 100

		case cmd_field_count is
			when 10 =>
				move_unit (
					module_cursor 	=> module,
					device_name		=> device_name,
					unit_name		=> unit_name,
					coordinates		=> coordinates,
					sheet			=> sheet,
					destination		=> destination,						
					log_threshold	=> log_threshold + 1);

			when 11 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end move_unit;
	

		
		

		
		
		
		
	procedure rotate_unit (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 8 =>
				rotate_unit (
					module_cursor 	=> module,
					device_name		=> to_device_name (get_field (cmd, 5)), -- IC1
					unit_name		=> to_unit_name (get_field (cmd, 6)), -- A
					coordinates		=> to_coordinates (get_field (cmd, 7)),  -- relative/absolute
					rotation		=> to_rotation (get_field (cmd, 8)), -- 90
					log_threshold	=> log_threshold + 1);

			when 9 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end rotate_unit;
			
		

		
		
		
		
		
		

	procedure fetch_unit (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	
		device_name : type_device_name;
		unit_name	: pac_unit_name.bounded_string;
		sheet		: type_sheet;
		place		: type_vector_model;
		rotation	: type_rotation;
	begin
		-- CS log message
		
		device_name := to_device_name (get_field (cmd, 5)); -- IC1
		unit_name	:= to_unit_name (get_field (cmd, 6)); -- C
		sheet		:= to_sheet (get_field (cmd, 7)); -- 1
		place		:= set (x => to_distance (get_field (cmd, 8)), -- 70
							y => to_distance (get_field (cmd, 9))); -- 100
	
		rotation	:= to_rotation (get_field (cmd, 10)); -- -90

		case cmd_field_count is
			when 10 =>
				fetch_unit (
					module_cursor	=> module,
					device_name		=> device_name,
					unit_name		=> unit_name,
					destination		=> to_position (place, sheet, rotation),
					log_threshold	=> log_threshold + 1);

			when 11 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end fetch_unit;
		


		
		
		
		
		
		
-- PLACEHOLDERS:

	procedure rotate_unit_placeholder (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_modes.schematic;
		use et_rotation_docu;
		use et_device_placeholders;
		meaning : type_placeholder_meaning;

		
		procedure do_it is begin
			case cmd_field_count is
				when 7 =>
					rotate_placeholder (
						module_cursor 	=> module,
						device_name		=> to_device_name (get_field (cmd, 5)), -- IC1
						unit_name		=> to_unit_name (get_field (cmd, 6)), -- A
						rotation		=> to_rotation_documentation (get_field (cmd, 7)), -- horizontal
						meaning			=> meaning,
						log_threshold	=> log_threshold + 1);

				when 8 .. type_field_count'last => 
					command_too_long (cmd, cmd_field_count - 1);
					
				when others => command_incomplete (cmd);
			end case;
		end do_it;
		
		
	begin
		-- CS log message
		
		case noun is
			when NOUN_NAME =>
				meaning := NAME;
				
			when NOUN_VALUE =>
				meaning := VALUE;
								
			when NOUN_PURPOSE =>
				meaning := PURPOSE;

			-- CS partcode ?

			when others => null; -- CS should never happen
		end case;

		do_it;		
	end rotate_unit_placeholder;


		
		
		
		
		
		
		
	procedure move_unit_placeholder (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_modes.schematic;
		use et_device_placeholders;
		meaning : type_placeholder_meaning;

		
		procedure do_it is begin
			case cmd_field_count is
				when 9 =>
					move_placeholder (
						module_cursor 	=> module,
						device_name		=> to_device_name (get_field (cmd, 5)), -- IC1
						unit_name		=> to_unit_name (get_field (cmd, 6)), -- A
						coordinates		=> to_coordinates (get_field (cmd, 7)),  -- relative/absolute
						point			=> to_vector_model (get_field (cmd, 8), get_field (cmd, 9)),
						meaning			=> meaning,
						log_threshold	=> log_threshold + 1);

				when 10 .. type_field_count'last => 
					command_too_long (cmd, cmd_field_count - 1);
					
				when others => command_incomplete (cmd);
			end case;
		end do_it;

		
	begin
		case noun is
			when NOUN_NAME =>
				meaning := NAME;
				
			when NOUN_VALUE =>
				meaning := VALUE;
								
			when NOUN_PURPOSE =>
				meaning := PURPOSE;

			-- CS partcode ?

			when others => null; -- CS should never happen
		end case;

		do_it;		
	end move_unit_placeholder;

		
end et_cp_schematic_unit;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

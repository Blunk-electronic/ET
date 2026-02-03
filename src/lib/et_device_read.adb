------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             DEVICE READ                                  --
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

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;

with ada.directories;
with ada.exceptions;

with et_directory_and_file_ops;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_schematic_geometry;			use et_schematic_geometry;
use et_schematic_geometry.pac_geometry_2;

with et_string_processing;
with et_file_sections;				use et_file_sections;
with et_symbol_read;				use et_symbol_read;
with et_conventions;

with et_port_names;
with et_symbol_name;				use et_symbol_name;
with et_symbol_ports;				use et_symbol_ports;
with et_symbol_model;				use et_symbol_model;
with et_symbol_library;				use et_symbol_library;
with et_device_partcode;			use et_device_partcode;
with et_device_appearance;			use et_device_appearance;
with et_device_model;				use et_device_model;
with et_device_value;
with et_device_prefix;
with et_units;
with et_unit_name;
with et_unit_swap_level;
with et_unit_add_level;

with et_package_variant_terminal_port_map;

with et_package_model_name;			use et_package_model_name;
with et_device_library;				use et_device_library;
with et_device_model_unit_internal;	use et_device_model_unit_internal;
with et_device_model_unit_external;	use et_device_model_unit_external;
with et_keywords;					use et_keywords;
with et_terminal_name;				use et_terminal_name;
with et_terminals;					use et_terminals;

with et_symbol_read_body;			use et_symbol_read_body;
with et_symbol_read_port;			use et_symbol_read_port;
with et_symbol_read_text;			use et_symbol_read_text;

with et_device_read_package_variant;	use et_device_read_package_variant;


package body et_device_read is
	

	procedure read_device (
		file_name 		: in pac_device_model_file.bounded_string; -- libraries/devices/7400.dev
		check_layers	: in et_pcb_stack.type_layer_check := (check => et_pcb_stack.NO);
		log_threshold	: in type_log_level) 
	is
		use et_string_processing;
		use et_device_value;
		use et_device_prefix;
		use et_unit_name;
		use et_package_variant_terminal_port_map;		
		
		file_handle : ada.text_io.file_type;

		line : type_fields_of_line;

		
		-- This is the sections stack of the device model:
		max_section_depth : constant positive := 6;
		
		package pac_sections_stack is new gen_pac_sections_stack (
			item	=> type_file_section,
			max 	=> max_section_depth);

		
		
		
		-- VARIABLES FOR TEMPORARILY STORAGE:
		prefix				: pac_device_prefix.bounded_string; -- T, IC
		value				: pac_device_value.bounded_string; -- BC548
		appearance			: type_appearance; -- virtual/pcb
		partcode			: pac_device_partcode.bounded_string; -- IC_PAC_S_SOT23_VAL_
		terminal_port_map	: pac_terminal_port_map.map;

		

		-- This procedure reads general meta information
		-- of the device model:
		-- - predefined prefix (like R, C, L)
		-- - appearance
		-- - predefined value
		-- - predefined partcode
		procedure read_meta (
			line			: in type_fields_of_line;
			log_threshold 	: in type_log_level)
		is
			kw : string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_prefix then -- prefix IC
				expect_field_count (line, 2);
				check_prefix_length (f (line, 2));
				prefix := to_prefix (f (line, 2));
				check_prefix_characters (prefix);
				log (text => "prefix " & to_string (prefix), level => log_threshold);
				
				if not et_conventions.prefix_valid (prefix) then
					--log (message_warning & "prefix of device model " &
					--	 to_string (file_name) & " not conformant with conventions !");
					null; -- CS output something helpful
				end if;

			elsif kw = keyword_value then -- value 7400
				expect_field_count (line, 2);

				-- validate value
				value := to_value_with_check (
					value						=> f (line, 2),
					error_on_invalid_character	=> false);

				if not value_characters_valid (value) then
					log (WARNING, "default value in device model " &
							to_string (file_name) & " contains invalid characters !");
					log_indentation_reset;
					value_invalid (to_string (value));
				end if;
				
				log (text => "value " & to_string (value), level => log_threshold);

			elsif kw = keyword_appearance then -- appearance virtual/pcb
				expect_field_count (line, 2);
				appearance := to_appearance (f (line, 2));
				log (text => "appearance " & to_string (appearance), level => log_threshold);								

			elsif kw = keyword_partcode then -- partcode IC_PAC_S_SO14_VAL_
				expect_field_count (line, 2);

				-- validate partcode length
				partcode := to_partcode (f (line, 2));
				
				log (text => "partcode " & to_string (partcode), level => log_threshold);
			else
				invalid_keyword (kw);
			end if;
		end read_meta;




		
		procedure insert_terminal (
			line : in type_fields_of_line)  -- terminal 14 unit 5 VCC
		is
			use et_units;
			use et_port_names;
			use pac_terminal_port_map;
			inserted	: boolean;
			position	: pac_terminal_port_map.cursor;

			terminal	: pac_terminal_name.bounded_string; -- H5, 14
			unit		: pac_unit_name.bounded_string; -- PWR, IO_BANK_2
			port		: pac_port_name.bounded_string; -- VCC

			place : type_field_count_positive := 1; -- the field being read from given line

			-- CS: detect missing parameters
			-- CS: warn about wrong misplaced keywords
			-- CS: test if terminal, unit and port exist
		begin
			while place <= get_field_count (line) loop
			
				-- We expect the terminal name after the keyword "terminal"
				if f (line, place) = keyword_terminal then
					terminal := to_terminal_name (f (line, place + 1)); -- 14

				-- After the keyword "unit" must come the unit name:
				elsif f (line, place) = keyword_unit then 
					unit := to_unit_name (f (line, place + 1)); -- 5

				-- After the keyword "port" must come the port name
				elsif f (line, place) = keyword_port then 
					port := to_port_name (f (line, place + 1)); -- VCC
					
				else
					invalid_keyword (f (line, place));
				end if;
					
				place := place + 2;
			end loop;

			-- insert terminal to port assigment in temporarily terminal_port_map
			insert (
				container	=> terminal_port_map,
				key			=> terminal, -- H5, 14
				inserted	=> inserted,
				position	=> position,
				new_item	=> (
								unit	=> unit, -- IO_BANK_2,
								name	=> port -- VCC
								));

			-- an assigment must be unique !
			if not inserted then
				log (ERROR, "terminal-to-port assigment already used !", console => true);
				raise constraint_error;
			end if;

			-- clean up for next terminal to port assigment
			terminal	:= to_terminal_name ("");
			unit		:= to_unit_name ("");
			port		:= to_port_name ("");
		end insert_terminal;


	


		
		-- This is the pointer that points to the possible
		-- internal symbol being read in the following:
		symbol_model : type_symbol_model_access;

		
		unit_name		: pac_unit_name.bounded_string; -- IO_BANK_2
		unit_position	: type_vector_model := origin; -- the position of the unit inside the device editor
		unit_swap_level	: et_unit_swap_level.type_swap_level := et_unit_swap_level.swap_level_default;
		unit_add_level	: et_unit_add_level.type_add_level := et_unit_add_level.add_level_default;
		units_internal	: pac_units_internal.map;
		units_external	: pac_units_external.map;

		unit_external 	: type_unit_external;
		unit_external_model_name : pac_symbol_model_name.bounded_string;
		
		
		
		procedure read_unit_internal (
			line : in type_fields_of_line)
		is
			kw : string := f (line, 1);
			use et_unit_swap_level;
			use et_unit_add_level;
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_name then
				expect_field_count (line, 2);
				unit_name := to_unit_name (f (line, 2));

				-- Create a new symbol where symbol_model is pointing at.
				-- The symbol assumes the appearance of the device.
				-- The symbol will be copied to the current unit later.
				case appearance is
					when APPEARANCE_VIRTUAL =>
						symbol_model := new type_symbol_model' (
							appearance	=> APPEARANCE_VIRTUAL,
							others		=> <>);

					when APPEARANCE_PCB =>
						symbol_model := new type_symbol_model' (
							appearance	=> APPEARANCE_PCB,
							others		=> <>);

					when others => 
						raise constraint_error; -- CS

				end case;
				
			elsif kw = keyword_position then -- position x 0.00 y 0.00
				expect_field_count (line, 5);

				-- extract unit position starting at field 2
				-- NOTE: this is the position of the unit inside the device editor !
				unit_position := to_vector_model (line, 2);

			elsif kw = keyword_swap_level then
				expect_field_count (line, 2);
				unit_swap_level := to_swap_level (f (line, 2));

			elsif kw = keyword_add_level then
				expect_field_count (line, 2);
				unit_add_level := to_add_level (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_unit_internal;
		

		
		
		-- Inserts in the temporarily collection of internal units a new unit.
		-- The symbol of the unit is the one accessed by pointer symbol_model.
		procedure insert_unit_internal is
			position : pac_units_internal.cursor;
			inserted : boolean;

			use pac_unit_name;
			use et_unit_swap_level;
			use et_unit_add_level;
		begin
			-- Depending on the appearance of the device, a unit with the same
			-- appearance is inserted in units_internal.
			case appearance is 
				when APPEARANCE_VIRTUAL =>
					pac_units_internal.insert (
						container	=> units_internal,
						position	=> position,
						inserted	=> inserted,
						key			=> unit_name,
						new_item	=> (
								appearance	=> APPEARANCE_VIRTUAL,
								symbol		=> symbol_model.all,
								position	=> unit_position,
								swap_level	=> unit_swap_level,
								add_level	=> unit_add_level));

				when APPEARANCE_PCB =>
					pac_units_internal.insert (
						container	=> units_internal,
						position	=> position,
						inserted	=> inserted,
						key			=> unit_name,
						new_item	=> (
								appearance	=> APPEARANCE_PCB,
								symbol		=> symbol_model.all,
								position	=> unit_position,
								swap_level	=> unit_swap_level,
								add_level	=> unit_add_level));

			end case;

			-- A unit name must occur only once. 
			-- Make sure the unit_name is not in use by any internal or external units:
			
			-- Test occurence in internal units:
			if not inserted then
				log (ERROR, "unit " & to_string (unit_name) 
					& " already used by another internal unit !", console => true);
				raise constraint_error;
			end if;

			-- Make sure the unit name is not in use by any external unit:
			if pac_units_external.contains (units_external, unit_name) then
				log (ERROR, "unit name " & to_string (unit_name) 
					& " already used by an external unit !", console => true);
				raise constraint_error;
			end if;
			
			-- clean up for next unit
			unit_name := to_unit_name ("");
			unit_position := origin;
			unit_swap_level := swap_level_default;
			unit_add_level := add_level_default;
			symbol_model := null;
			
		end insert_unit_internal;

		
		
		
		
		
		procedure read_unit_external (
			line : in type_fields_of_line)
		is
			use et_unit_swap_level;
			use et_unit_add_level;
			kw : string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_name then -- name A, B, ...
				expect_field_count (line, 2);
				unit_name := to_unit_name (f (line, 2));

			elsif kw = keyword_position then -- position x 0.00 y 0.00
				expect_field_count (line, 5);

				-- extract unit position starting at field 2
				-- NOTE: this is the position of the unit inside the device editor !
				unit_external.position := to_vector_model (line, 2);

			elsif kw = keyword_swap_level then -- swap_level 1
				expect_field_count (line, 2);
				unit_external.swap_level := to_swap_level (f (line, 2));

			elsif kw = keyword_add_level then -- add_level next
				expect_field_count (line, 2);
				unit_external.add_level := to_add_level (f (line, 2));

			elsif kw = keyword_symbol_file then -- symbol_model libraries/symbols/nand.sym
				expect_field_count (line, 2);
				unit_external_model_name := to_file_name (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_unit_external;
		
		

		
		
		-- Inserts in the temporarily collection of external units a new unit.
		procedure insert_unit_external is
			use pac_unit_name;
			
			-- CS log messages
			
			
			procedure read_symbol_model is
			begin
				-- read the symbol model (like ../libraries/symbols/power_gnd.sym)
				read_symbol (unit_external_model_name,
					log_threshold + 1);			
			end read_symbol_model;
			
			
			
			inserted : boolean;
			
			-- Adds the internal unit to the device:
			procedure add_to_device is 
				cursor : pac_units_external.cursor;
			begin
				pac_units_external.insert (
					container	=> units_external,
					position	=> cursor,
					inserted	=> inserted,
					key			=> unit_name,
					new_item	=> unit_external);
			end add_to_device;
			
			
			
			-- Tests the "inserted" flag and issues a log message.
			-- The inserted-flag indicates that the unit does not exist
			-- already:			
			procedure check_for_name_in_use is begin
				-- A unit name must occur only once. 
				-- Make sure the unit_name is not in use by any internal or external units:

				-- Test occurence in external units:
				if not inserted then
					log (ERROR, "unit name " & to_string (unit_name) 
						& " already used by another external unit !");
					raise constraint_error;
				end if;

				-- Make sure the unit name is not in use by any internal unit:
				if pac_units_internal.contains (units_internal, unit_name) then
					log (ERROR, "unit name " & to_string (unit_name) 
						& " already used by an internal unit !");
					raise constraint_error;
				end if;			
			end check_for_name_in_use;
			
			
			
			procedure clean_up is begin
				-- clean up for next unit
				unit_name := to_unit_name ("");
				-- CS unit_external_model_name := 
				unit_external := (others => <>);		
			end clean_up;
			
			
		begin
			
			read_symbol_model;			
			
			-- Get the cursor to the symbol model:
			unit_external.model_cursor := get_symbol_model (unit_external_model_name);
			
			add_to_device;
			
			check_for_name_in_use;

			clean_up;
		end insert_unit_external;
		
		

		
		
		
		procedure process_line is 
			
			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
				
			begin
				case pac_sections_stack.current is

					when SEC_VARIANTS | SEC_UNITS_INTERNAL | SEC_UNITS_EXTERNAL => 
						case pac_sections_stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;
						
					when SEC_VARIANT =>
						case pac_sections_stack.parent is
							when SEC_VARIANTS => insert_package_variant;
							when others => invalid_section;
						end case;

						
					when SEC_TERMINAL_PORT_MAP =>
						case pac_sections_stack.parent is
							when SEC_VARIANT =>
								-- copy temporarily terminal_port_map to current variant
								variant.terminal_port_map := terminal_port_map;

								-- clean up temporarily terminal_port_map for next variant
								pac_terminal_port_map.clear (terminal_port_map);
							when others => invalid_section;
						end case;

						
					when SEC_UNIT =>
						case pac_sections_stack.parent is
							when SEC_UNITS_INTERNAL => insert_unit_internal;									
							when SEC_UNITS_EXTERNAL => insert_unit_external;								
							when others => invalid_section;
						end case;
						
					when SEC_SYMBOL =>
						case pac_sections_stack.parent is
							when SEC_UNIT => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_DRAW =>
						case pac_sections_stack.parent is
							when SEC_SYMBOL => null;  -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case pac_sections_stack.parent is
							when SEC_DRAW => insert_body_line (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case pac_sections_stack.parent is
							when SEC_DRAW => insert_body_arc (symbol_model, log_threshold + 1);								
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case pac_sections_stack.parent is
							when SEC_DRAW => insert_body_circle (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_TEXTS =>
						case pac_sections_stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case pac_sections_stack.parent is
							when SEC_TEXTS => insert_text (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDERS =>
						case pac_sections_stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case pac_sections_stack.parent is
							when SEC_PLACEHOLDERS => insert_placeholder (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_PORTS =>
						case pac_sections_stack.parent is 
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PORT =>
						case pac_sections_stack.parent is
							when SEC_PORTS => insert_port (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen

					when others => invalid_section;
				end case;

			end execute_section;

			
			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [UNIT
				section			: in type_file_section) -- SEC_UNIT
				return boolean 
			is begin
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						pac_sections_stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
						return true;

					elsif f (line, 2) = section_end then -- section footer detected in field 2

						-- The section name in the footer must match the name
						-- of the current section. Otherwise abort.
						if section /= pac_sections_stack.current then
							log_indentation_reset;
							invalid_section;
						end if;
						
						-- Now that the section ends, the data collected in temporarily
						-- variables is processed.
						execute_section;
						
						pac_sections_stack.pop;
						if pac_sections_stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (pac_sections_stack.current), level => log_threshold + 3);
						end if;
						return true;

					else
						log (ERROR, write_missing_begin_end, console => true);
						raise constraint_error;
					end if;

				else -- neither a section header nor footer
					return false;
				end if;
			end set;

			
		begin -- process_line
			if set (section_variants, SEC_VARIANTS) then null;
			elsif set (section_variant, SEC_VARIANT) then null;
			elsif set (section_terminal_port_map, SEC_TERMINAL_PORT_MAP) then null;
			elsif set (section_units_internal, SEC_UNITS_INTERNAL) then null;
			elsif set (section_units_external, SEC_UNITS_EXTERNAL) then null;			
			elsif set (section_unit, SEC_UNIT) then null;
			elsif set (section_symbol, SEC_SYMBOL) then null;
			elsif set (section_draw, SEC_DRAW) then null;			
			elsif set (section_line, SEC_LINE) then null;								
			elsif set (section_arc, SEC_ARC) then null;								
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_texts, SEC_TEXTS) then null;
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_ports, SEC_PORTS) then null;
			elsif set (section_port, SEC_PORT) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "device line --> " & to_string (line), level => log_threshold + 3);
		
				case pac_sections_stack.current is
					when SEC_INIT => read_meta (line, log_threshold + 1);						
						
					when SEC_VARIANTS | SEC_UNITS_INTERNAL | SEC_UNITS_EXTERNAL => 
						case pac_sections_stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;
						
					when SEC_VARIANT =>
						case pac_sections_stack.parent is
							when SEC_VARIANTS => read_package_variant (line, check_layers, log_threshold + 1);
							when others => invalid_section;
						end case;

						
					when SEC_TERMINAL_PORT_MAP =>
						case pac_sections_stack.parent is
							when SEC_VARIANT =>
								expect_field_count (line, 6); -- terminal 14 unit 5 port VCC

								-- extract terminal to port assignment
								insert_terminal (line);
							
							when others => invalid_section;
						end case;

						
					when SEC_UNIT =>
						case pac_sections_stack.parent is
							when SEC_UNITS_INTERNAL => read_unit_internal (line);
							when SEC_UNITS_EXTERNAL => read_unit_external (line);
							when others => invalid_section;
						end case;

						
					when SEC_SYMBOL =>
						case pac_sections_stack.parent is
							when SEC_UNIT =>
								case pac_sections_stack.parent (degree => 2) is
									when SEC_UNITS_INTERNAL => null;
									when others => invalid_section;
								end case;
								
							when others => invalid_section;
						end case;

						
					when SEC_DRAW =>
						case pac_sections_stack.parent is
							when SEC_SYMBOL => null;  -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case pac_sections_stack.parent is
							when SEC_DRAW => read_body_line (line);
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case pac_sections_stack.parent is
							when SEC_DRAW => read_body_arc (line);
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case pac_sections_stack.parent is
							when SEC_DRAW => read_body_circle (line);
							when others => invalid_section;
						end case;
						
					when SEC_TEXTS =>
						case pac_sections_stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case pac_sections_stack.parent is
							when SEC_TEXTS => read_text (line);
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDERS =>
						case pac_sections_stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case pac_sections_stack.parent is
							when SEC_PLACEHOLDERS => read_placeholder (line);
							when others => invalid_section;
						end case;
						
					when SEC_PORTS =>
						case pac_sections_stack.parent is 
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PORT =>
						case pac_sections_stack.parent is
							when SEC_PORTS => read_port (line);
							when others => invalid_section;
						end case;
						
					when others => invalid_section;
				end case;
			end if;

			
			exception when event: others =>
				log (text => "file " & to_string (file_name) & space 
					 & get_affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;

		previous_input : ada.text_io.file_type renames current_input;

		
	begin -- read_device_file
		log_indentation_up;
		log (text => "reading device model " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- test if container device_library already contains a model
		-- named "file_name". If so, there would be no need to read the file_name again.
		if pac_device_models.contains (device_library, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			-- If the model file is to be read, first check if the file exists.
			declare
				use et_directory_and_file_ops;
				file : string := expand (to_string (file_name));
			begin
				if ada.directories.exists (file) then

					-- open device model file
					open (
						file => file_handle,
						mode => in_file, 
						name => file);

				else
					log (ERROR, "device model " & file &
						 " not found !", console => true);
					raise constraint_error;
				end if;
			end;

			set_input (file_handle);
			
			-- Init section pac_sections_stack.
			pac_sections_stack.init;
			pac_sections_stack.push (SEC_INIT);

			-- read the file line by line
			while not end_of_file loop
				line := read_line (
					line 			=> get_line,
					number			=> positive (ada.text_io.line (current_input)),
					comment_mark 	=> comment_mark_default,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if get_field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if pac_sections_stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Assemble final device and insert it in device_library:
			case appearance is
				when APPEARANCE_PCB => -- a real device

					-- If a value was specified (via an entry like "value 100R),
					-- check if it meets certain conventions regarding its prefix.
					-- The prefix gives information about the category of the device:
					if pac_device_value.length (value) > 0 then
						if not et_conventions.value_valid (value, prefix) then
							log (WARNING, "default value of device model " &
								to_string (file_name) & 
								" not conformant with conventions !");
						end if;
					end if;

					pac_device_models.insert (
						container	=> device_library, 
						key			=> file_name, -- libraries/devices/7400.dev
						new_item	=> (
								appearance		=> APPEARANCE_PCB,
								prefix			=> prefix, -- IC
								units_internal	=> units_internal,
								units_external	=> units_external,
								value			=> value,
								--partcode		=> partcode,
								variants		=> variants));

				when APPEARANCE_VIRTUAL =>
					pac_device_models.insert (
						container	=> device_library, 
						key			=> file_name, -- libraries/devices/power_gnd.dev
						new_item	=> (
								appearance		=> APPEARANCE_VIRTUAL,
								prefix			=> prefix, -- PWR
								units_internal	=> units_internal,
								units_external	=> units_external));

			end case;
		end if;

		-- CS Check integrity of device: port terminal map, positions of units, ...
		-- (style guides, conventions ...)
		-- use function "last" to fetch latest device

		log_indentation_down;
		log_indentation_down;		

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;

	end read_device;

	
end et_device_read;

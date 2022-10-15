------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICE READ AND WRITE                            --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;
-- with ada.tags;

with ada.directories;
with ada.exceptions;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_material;
with et_general;				--use et_general;

with et_coordinates;			use et_coordinates;
use et_coordinates.pac_geometry_2;

with et_string_processing;
with et_general_rw;				use et_general_rw;
with et_schematic_rw;			use et_schematic_rw;
with et_symbol_rw;				use et_symbol_rw;
with et_pcb_rw.device_packages;
with et_conventions;
with et_geometry;				use et_geometry;
with et_text;
with et_symbols;				use et_symbols;
with et_packages;				use et_packages;

package body et_device_rw is

	use et_terminals;
	
	procedure create_device (
	-- Creates a device and stores it in container et_devices.devices.
		device_name		: in pac_device_model_file.bounded_string; -- libraries/devices/7400.dev
		appearance		: in et_symbols.type_appearance;
		log_threshold	: in type_log_level) 
	is
		use et_string_processing;
		use pac_devices_lib;
	begin
		log (text => "creating device " & to_string (device_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "appearance " & to_string (appearance) & " ...", level => log_threshold);
		
		-- Test if device already exists. If already exists, issue warning and exit.
		if contains (devices, device_name) then
			log (WARNING, text => "device already exists -> skipped", level => log_threshold + 1);
		else
			case appearance is
				when PCB =>
					insert (
						container	=> devices,
						key			=> device_name,
						new_item	=> (appearance => PCB, others => <>)
						);

				when VIRTUAL =>
					insert (
						container	=> devices,
						key			=> device_name,
						new_item	=> (appearance => VIRTUAL, others => <>)
						);
			end case;					
		end if;

		log_indentation_down;
	end create_device;

	
	procedure save_device (
	-- Saves the given device model in a file specified by name.
		file_name		: in pac_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev
		device			: in type_device_lib; -- the actual device model
		log_threshold	: in type_log_level)
	is
		use et_string_processing;
		file_handle : ada.text_io.file_type;

		use pac_variants;
		variant_cursor : pac_variants.cursor;
		
		procedure write_variant (
			packge	: in pac_package_variant_name.bounded_string;
			variant	: in type_variant) is
			use pac_terminal_port_map;	

			procedure write_terminal (terminal_cursor : in pac_terminal_port_map.cursor) is begin
				write (keyword => keyword_terminal, parameters => 
					space & to_string (key (terminal_cursor)) & space -- terminal name like G14 or 16
					& keyword_unit & space & to_string (element (terminal_cursor).unit) -- unit name like A,B or GPIO_BANK_1
					& space & keyword_port & space & to_string (element (terminal_cursor).name) 	-- port name like CE, WE, GND
					);
			end write_terminal;

		begin -- write_variant
			write (keyword => keyword_package_model, parameters => to_string (variant.package_model)); -- CS path correct ??
			section_mark (section_terminal_port_map, HEADER);
			iterate (variant.terminal_port_map, write_terminal'access);
			section_mark (section_terminal_port_map, FOOTER);						
		end write_variant;

		use pac_units_internal;
		unit_internal_cursor : pac_units_internal.cursor := device.units_internal.first;
		
		use pac_units_external;
		unit_external_cursor : pac_units_external.cursor := device.units_external.first;

		procedure query_internal_unit (
			name	: in pac_unit_name.bounded_string;
			unit	: in type_unit_internal) is
		begin -- query_internal_unit
			write (keyword => keyword_name, parameters => to_string (name));
			write (keyword => keyword_position, parameters => position (unit.position));
			write (keyword => keyword_swap_level, parameters => to_string (unit.swap_level));
			write (keyword => keyword_add_level , parameters => to_string (unit.add_level));
			section_mark (section_symbol, HEADER);
			write_symbol (unit.symbol, log_threshold + 1);
			section_mark (section_symbol, FOOTER);
		end query_internal_unit;

		procedure query_external_unit (
			name	: in pac_unit_name.bounded_string;
			unit	: in type_unit_external) is
		begin -- query_external_unit
			write (keyword => keyword_name, parameters => to_string (name));
			write (keyword => keyword_position, parameters => position (unit.position));
			write (keyword => keyword_swap_level, parameters => to_string (unit.swap_level));
			write (keyword => keyword_add_level , parameters => to_string (unit.add_level));
			write (keyword => keyword_symbol_file, parameters => to_string (unit.model));
		end query_external_unit;
		
	begin -- save_device
		log (text => to_string (file_name), level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & et_general.system_name & " device");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;

		reset_tab_depth;

		-- prefix, appearance ...
		write (keyword => keyword_prefix, parameters => to_string (device.prefix));
		write (keyword => keyword_appearance, parameters => to_string (device.appearance));

		-- package variants
		case device.appearance is
			when PCB =>
				write (keyword => keyword_value, parameters => to_string (device.value));
				--write (keyword => keyword_partcode, parameters => to_string (device.partcode));

				section_mark (section_variants, HEADER);

				variant_cursor := device.variants.first;
				while variant_cursor /= pac_variants.no_element loop
					section_mark (section_variant, HEADER);
					write (keyword => keyword_name, parameters => to_string (key (variant_cursor)));

					query_element (
						position	=> variant_cursor,
						process		=> write_variant'access);

					section_mark (section_variant, FOOTER);					
					next (variant_cursor);
				end loop;

				section_mark (section_variants, FOOTER);
				
			when others => null;				
		end case;

		-- internal units
		section_mark (section_units_internal, HEADER);
		while unit_internal_cursor /= pac_units_internal.no_element loop
			section_mark (section_unit, HEADER);
			query_element (unit_internal_cursor, query_internal_unit'access);
			section_mark (section_unit, FOOTER);
			next (unit_internal_cursor);
		end loop;
		section_mark (section_units_internal, FOOTER);

		-- external units
		section_mark (section_units_external, HEADER);
		while unit_external_cursor /= pac_units_external.no_element loop
			section_mark (section_unit, HEADER);
			query_element (unit_external_cursor, query_external_unit'access);
			section_mark (section_unit, FOOTER);
			next (unit_external_cursor);
		end loop;
		section_mark (section_units_external, FOOTER);

		-- write footer
		new_line;		
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " device model file end");
		new_line;

		reset_tab_depth;
		
		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end save_device;

	procedure read_device (
	-- Opens the device and stores it in container devices.
		file_name 		: in pac_device_model_file.bounded_string; -- libraries/devices/7400.dev
		check_layers	: in et_pcb_stack.type_layer_check := (check => et_pcb_stack.NO);
		log_threshold	: in type_log_level) 
	is
		use et_string_processing;
		use et_text;
		file_handle : ada.text_io.file_type;

		line : type_fields_of_line;

		-- This is the section stack of the device model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 6;
		package stack is new et_general_rw.stack_lifo (
			item	=> type_section,
			max 	=> max_section_depth);

		function to_string (section : in type_section) return string is
		-- Converts a section like SEC_VARIANT to a string "variant".
			len : positive := type_section'image (section)'length;
		begin
			return to_lower (type_section'image (section) (5..len));
		end to_string;
		
		-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
		prefix				: pac_device_prefix.bounded_string; -- T, IC
		value				: pac_device_value.bounded_string; -- BC548
		appearance			: type_appearance; -- virtual/pcb
		partcode			: et_material.type_partcode.bounded_string; -- IC_PAC_S_SOT23_VAL_
		variant				: type_variant;
		variant_name		: pac_package_variant_name.bounded_string; -- N, D
		variants			: pac_variants.map;
		terminal_port_map	: pac_terminal_port_map.map;

		procedure insert_terminal (line : in type_fields_of_line) is -- terminal 14 unit 5 VCC
			use pac_terminal_port_map;
			inserted	: boolean;
			position	: pac_terminal_port_map.cursor;

			terminal	: pac_terminal_name.bounded_string; -- H5, 14
			unit		: pac_unit_name.bounded_string; -- PWR, IO_BANK_2
			port		: pac_port_name.bounded_string; -- VCC

			place : count_type := 1; -- the field being read from given line

			-- CS: detect missing parameters
			-- CS: warn about wrong misplaced keywords
			-- CS: test if terminal, unit and port exist
		begin
			while place <= field_count (line) loop
			
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

		procedure insert_variant is
			use pac_variants;
			inserted : boolean;
			position : pac_variants.cursor;
		begin
			check_variant_name_characters (variant_name);

			insert (
				container	=> variants,
				key			=> variant_name, -- N, D 
				inserted	=> inserted,
				position	=> position,
				new_item	=> variant);

			-- A particular variant must occur only once in the device model:
			if not inserted then
				log (ERROR, "variant " & to_string (variant_name) & " already used !", console => true);
				raise constraint_error;
			end if;

			-- Read package model (like libraries/packages/__#__#lbr#bel_ic_pretty#S_SO14.pac)
			-- and do a conductor layer check if required.
			et_pcb_rw.device_packages.read_package (variant.package_model, check_layers, log_threshold + 1);

			-- clean up for next variant
			variant := (others => <>);
		end insert_variant;

		unit_name			: pac_unit_name.bounded_string; -- IO_BANK_2
		unit_position		: type_point := origin; -- the position of the unit inside the device editor
		unit_swap_level		: type_swap_level := swap_level_default;
		unit_add_level		: type_add_level := add_level_default;
		unit_symbol			: access type_symbol;
		units_internal		: pac_units_internal.map;
		units_external		: pac_units_external.map;

		-- CS move to et_schematic_rw ? wrong ? better to et_symbol_rw ?
		symbol_line			: et_symbols.type_line;
		symbol_arc			: et_symbols.type_arc;
		symbol_circle		: et_symbols.type_circle;
		symbol_text_base	: type_text_basic;
		
		symbol_text_position		: type_point;
		symbol_text_content			: et_text.pac_text_content.bounded_string;
		symbol_placeholder_meaning	: type_placeholder_meaning := placeholder_meaning_default;
		
		port					: type_port_base;
		port_name				: pac_port_name.bounded_string;
		port_direction			: type_port_direction := port_direction_default;
		port_sensitivity_edge	: type_sensitivity_edge := sensitivity_edge_default;
		port_sensitivity_level	: type_sensitivity_level := sensitivity_level_default;
		port_output_inverted	: type_output_inverted := output_inverted_default;
		port_output_tristate	: type_output_tristate := output_tristate_default;
		port_output_weakness	: type_output_weakness := output_weakness_default;
		port_power_level		: type_power_level := port_power_level_default;

		unit_external : type_unit_external;
		
		procedure insert_unit_internal is
		-- Inserts in the temporarily collection of internal units a new unit.
		-- The symbol of the unit is the one accessed by pointer unit_symbol.
			position : pac_units_internal.cursor;
			inserted : boolean;
		begin
			-- Depending on the appearance of the device, a unit with the same
			-- appearance is inserted in units_internal.
			case appearance is 
				when VIRTUAL =>
					pac_units_internal.insert (
						container	=> units_internal,
						position	=> position,
						inserted	=> inserted,
						key			=> unit_name,
						new_item	=> (
								appearance	=> VIRTUAL,
								symbol		=> unit_symbol.all,
								position	=> unit_position,
								swap_level	=> unit_swap_level,
								add_level	=> unit_add_level));

				when PCB =>
					pac_units_internal.insert (
						container	=> units_internal,
						position	=> position,
						inserted	=> inserted,
						key			=> unit_name,
						new_item	=> (
								appearance	=> PCB,
								symbol		=> unit_symbol.all,
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
			unit_symbol := null;
			
		end insert_unit_internal;

		procedure insert_unit_external is
		-- Inserts in the temporarily collection of external units a new unit.
			position : pac_units_external.cursor;
			inserted : boolean;
		begin
			pac_units_external.insert (
				container	=> units_external,
				position	=> position,
				inserted	=> inserted,
				key			=> unit_name,
				new_item	=> unit_external);

			-- A unit name must occur only once. 
			-- Make sure the unit_name is not in use by any internal or external units:

			-- Test occurence in external units:
			if not inserted then
				log (ERROR, "unit name " & to_string (unit_name) 
					& " already used by another external unit !", console => true);
				raise constraint_error;
			end if;

			-- Make sure the unit name is not in use by any internal unit:
			if pac_units_internal.contains (units_internal, unit_name) then
				log (ERROR, "unit name " & to_string (unit_name) 
					& " already used by an internal unit !", console => true);
				raise constraint_error;
			end if;

			-- read the symbol model (like ../libraries/symbols/power_gnd.sym)
			read_symbol (unit_external.model, log_threshold + 1);

			-- clean up for next unit
			unit_name := to_unit_name ("");
			unit_external := (others => <>);
		end insert_unit_external;

		procedure insert_port is 
			inserted	: boolean;
			cursor		: pac_ports.cursor;
		begin
			case port_direction is
				when PASSIVE =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> PASSIVE)
						);

				when INPUT_ANALOG =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_ANALOG)
						);

				when INPUT_DIGITAL =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_DIGITAL,
							sensitivity_edge		=> port_sensitivity_edge,
							sensitivity_level		=> port_sensitivity_level)
						);

				when OUTPUT_ANALOG =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> OUTPUT_ANALOG,
							output_analog_tristate	=> port_output_tristate,
							output_analog_weakness	=> port_output_weakness)
						);

				when OUTPUT_DIGITAL =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> OUTPUT_DIGITAL,
							output_digital_inverted	=> port_output_inverted,
							output_digital_tristate	=> port_output_tristate,
							output_digital_weakness	=> port_output_weakness)
						);

				when BIDIR_DIGITAL =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> BIDIR_DIGITAL,
							output_inverted			=> port_output_inverted,
							output_tristate			=> port_output_tristate,
							output_weakness			=> port_output_weakness,
							input_sensitivity_edge	=> port_sensitivity_edge,
							input_sensitivity_level	=> port_sensitivity_level)
						);

				when POWER_OUT =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_OUT,
							level					=> port_power_level)
						);

				when POWER_IN =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_IN,
							level					=> port_power_level)
						);

				when NOT_CONNECTED =>
					pac_ports.insert (
						container	=> unit_symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> NOT_CONNECTED)
						);
			end case;

			-- abort if port name already used:
			if not inserted then
				log (ERROR, "port " & to_string (port_name) & " already in use !", console => true);
				raise constraint_error;
			end if;
			
			-- reset port parameters for next port
			port					:= (others => <>);
			port_name				:= to_port_name ("");
			port_direction			:= port_direction_default;
			port_sensitivity_edge	:= sensitivity_edge_default;
			port_sensitivity_level	:= sensitivity_level_default;
			port_output_inverted	:= output_inverted_default;
			port_output_tristate	:= output_tristate_default;
			port_output_weakness	:= output_weakness_default;
			port_power_level		:= port_power_level_default;

		end insert_port;
		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
				
			begin -- execute_section
				case stack.current is

					when SEC_VARIANTS | SEC_UNITS_INTERNAL | SEC_UNITS_EXTERNAL => 
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_VARIANT =>
						case stack.parent is
							when SEC_VARIANTS =>
								-- insert the temporarily variant in the collection of variants
								insert_variant;

							when others => invalid_section;
						end case;

					when SEC_TERMINAL_PORT_MAP =>
						case stack.parent is
							when SEC_VARIANT =>
								-- copy temporarily terminal_port_map to current variant
								variant.terminal_port_map := terminal_port_map;

								-- clean up temporarily terminal_port_map for next variant
								pac_terminal_port_map.clear (terminal_port_map);
							when others => invalid_section;
						end case;

					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS_INTERNAL =>
								insert_unit_internal;
									
							when SEC_UNITS_EXTERNAL =>
								insert_unit_external;
								
							when others => invalid_section;
						end case;

					when SEC_SYMBOL =>
						case stack.parent is
							when SEC_UNIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_DRAW =>
						case stack.parent is
							when SEC_SYMBOL => null;  -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_DRAW => 

								-- append symbol_line to unit_symbol
								pac_lines.append (
									container	=> unit_symbol.shapes.lines,
									new_item	=> symbol_line);

								-- clean up for next line
								symbol_line := (others => <>);
								
							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_arc to unit_symbol
								pac_arcs.append (
									container	=> unit_symbol.shapes.arcs,
									new_item	=> symbol_arc);

								-- clean up for next arc
								symbol_arc := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_circle to unit_symbol
								pac_circles.append (
									container	=> unit_symbol.shapes.circles,
									new_item	=> symbol_circle);

								-- clean up for next circle
								symbol_circle := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>

								-- append symbol text to symbol
								pac_texts.append (
									container	=> unit_symbol.texts,
									new_item	=> (symbol_text_base with
										content		=> symbol_text_content,
										position	=> symbol_text_position));

								-- clean up for next symbol text
								symbol_text_base := (others => <>);
								symbol_text_content := to_content ("");
								symbol_text_position := origin;
								
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>

								-- Assign symbol text placeholder to symbol.
								-- The meaning of the placeholder determines where
								-- the placeholder is to be assigned. 
								-- If meaning is not specified in section PLACEHOLDER,
								-- the default meaning is assumed which raise an error.

								-- CS: warn if placeholder exists multiple times. The latest
								-- placeholder would overwrite the previous one.

								case symbol_placeholder_meaning is
									when NAME =>
										unit_symbol.name := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when et_symbols.VALUE =>
										unit_symbol.value := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when PURPOSE =>
										unit_symbol.purpose := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									-- Default meaning causes an error:
									when others => 
										log (ERROR, "meaning of placeholder not specified !",
											 console => true);
										raise constraint_error;
								end case;

								-- clean up for next symbol text placeholder
								symbol_text_base := (others => <>);
								symbol_text_position := origin;
								symbol_placeholder_meaning := placeholder_meaning_default;
							
							when others => invalid_section;
						end case;

					when SEC_PORTS =>
						case stack.parent is 
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS => insert_port;
							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen
				end case;

			end execute_section;

			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [UNIT
				section			: in type_section) -- SEC_UNIT
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
						return true;

					elsif f (line, 2) = section_end then -- section footer detected in field 2

						-- The section name in the footer must match the name
						-- of the current section. Otherwise abort.
						if section /= stack.current then
							log_indentation_reset;
							invalid_section;
						end if;
						
						-- Now that the section ends, the data collected in temporarily
						-- variables is processed.
						execute_section;
						
						stack.pop;
						if stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 3);
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
		
				case stack.current is

					when SEC_INIT =>
						declare
							kw : string := f (line, 1);
						begin
							-- CS: In the following: set a corresponding parameter-found-flag
							if kw = keyword_prefix then -- prefix IC
								expect_field_count (line, 2);
								check_prefix_length (f (line,2));
								prefix := to_prefix (f (line,2));
								check_prefix_characters (prefix);
								log (text => "prefix " & to_string (prefix), level => log_threshold + 1);
								
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
								
								log (text => "value " & to_string (value), level => log_threshold + 1);

							elsif kw = keyword_appearance then -- appearance virtual/pcb
								expect_field_count (line, 2);
								appearance := to_appearance (f (line,2));
								log (text => "appearance " & to_string (appearance), level => log_threshold + 1);								

							elsif kw = et_material.keyword_partcode then -- partcode IC_PAC_S_SO14_VAL_
								expect_field_count (line, 2);

								-- validate partcode length
								partcode := et_material.to_partcode (f (line,2));
								
								log (text => "partcode " & et_material.to_string (partcode), level => log_threshold + 1);
							else
								invalid_keyword (kw);
							end if;
						end;

					when SEC_VARIANTS | SEC_UNITS_INTERNAL | SEC_UNITS_EXTERNAL => 
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_VARIANT =>
						case stack.parent is
							when SEC_VARIANTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name D
										expect_field_count (line, 2);
										check_variant_name_length (f (line, 2));
										variant_name := to_variant_name (f (line,2));
										log (text => "variant " & to_string (variant_name), level => log_threshold + 1);
										
									elsif kw = keyword_package_model then -- package_model libraries/packages/S_SO14.pac
										expect_field_count (line, 2);

										-- The given path is something like libraries/packages/S_SO14.pac.
										-- Check if the package name like S_SO14 is too long or contains invalid characters.
										check_package_name_length (ada.directories.base_name (f (line, 2)));
										check_package_name_characters (to_package_name (ada.directories.base_name (f (line, 2))));

										variant.package_model := to_file_name (f (line,2));
										log (text => "package model " & to_string (variant.package_model), level => log_threshold + 1);
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_TERMINAL_PORT_MAP =>
						case stack.parent is
							when SEC_VARIANT =>
								expect_field_count (line, 6); -- terminal 14 unit 5 port VCC

								-- extract terminal to port assignment
								insert_terminal (line);
							
							when others => invalid_section;
						end case;

					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS_INTERNAL =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then
										expect_field_count (line, 2);
										unit_name := to_unit_name (f (line,2));

										-- Create a new symbol where unit_symbol is pointing at.
										-- The symbol assumes the appearance of the device.
										-- The symbol will be copied to the current unit later.
										case appearance is
											when VIRTUAL =>
												unit_symbol := new type_symbol' (
													appearance	=> VIRTUAL,
													others		=> <>);

											when PCB =>
												unit_symbol := new type_symbol' (
													appearance	=> PCB,
													others		=> <>);

											when others => 
												raise constraint_error; -- CS

										end case;
										
									elsif kw = keyword_position then -- position x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract unit position starting at field 2
										-- NOTE: this is the position of the unit inside the device editor !
										unit_position := to_position (line, 2);

									elsif kw = keyword_swap_level then
										expect_field_count (line, 2);
										unit_swap_level := to_swap_level (f (line, 2));

									elsif kw = keyword_add_level then
										expect_field_count (line, 2);
										unit_add_level := to_add_level (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when SEC_UNITS_EXTERNAL =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name A, B, ...
										expect_field_count (line, 2);
										unit_name := to_unit_name (f (line,2));

									elsif kw = keyword_position then -- position x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract unit position starting at field 2
										-- NOTE: this is the position of the unit inside the device editor !
										unit_external.position := to_position (line, 2);

									elsif kw = keyword_swap_level then -- swap_level 1
										expect_field_count (line, 2);
										unit_external.swap_level := to_swap_level (f (line, 2));

									elsif kw = keyword_add_level then -- add_level next
										expect_field_count (line, 2);
										unit_external.add_level := to_add_level (f (line, 2));

									elsif kw = keyword_symbol_file then -- symbol_model libraries/symbols/nand.sym
										expect_field_count (line, 2);
										unit_external.model := to_file_name (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_SYMBOL =>
						case stack.parent is
							when SEC_UNIT =>
								case stack.parent (degree => 2) is
									when SEC_UNITS_INTERNAL => null;
									when others => invalid_section;
								end case;
								
							when others => invalid_section;
						end case;

					when SEC_DRAW =>
						case stack.parent is
							when SEC_SYMBOL => null;  -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_line.start_point := to_position (line,2);
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										symbol_line.end_point := to_position (line,2);

									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_line.width := to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_arc.center := to_position (line,2);

									elsif kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_arc.start_point := to_position (line,2);
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										symbol_arc.end_point := to_position (line,2);

									elsif kw = et_geometry.keyword_direction then -- direction ccw
										expect_field_count (line, 2);

										symbol_arc.direction := to_direction (f (line, 2));

									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_arc.width := to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_circle.center := to_position (line,2);

									elsif kw = keyword_width then -- widht 0.2
										expect_field_count (line, 2);
										symbol_circle.width := to_distance (f (line, 2));

									elsif kw = keyword_radius then -- radius 5
										expect_field_count (line, 2);
										symbol_circle.radius := to_radius (f (line, 2));

									elsif kw = keyword_filled then -- filled yes/no
										expect_field_count (line, 2);
										symbol_circle.filled := to_circle_filled (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the text position starting at field 2
										symbol_text_position := to_position (line,2);

									elsif kw = keyword_content then -- content "dummy NAND gate"
										expect_field_count (line, 2);
										symbol_text_content := et_text.to_content (f (line, 2));

									elsif kw = et_text.keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

-- 									elsif kw = et_text.keyword_line_width then -- line_width 0.2
-- 										expect_field_count (line, 2);
-- 										symbol_text_base.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := et_symbols.pac_text_schematic.to_rotation_doc (f (line, 2));
										
-- 									elsif kw = keyword_style then -- style italic
-- 										expect_field_count (line, 2);
-- 										symbol_text_base.style := to_text_style (f (line, 2));

									elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := et_text.to_alignment (line, 2);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the placeholder position starting at field 2
										symbol_text_position := to_position (line,2);

									elsif kw = keyword_meaning then -- meaning reference
										expect_field_count (line, 2);
										symbol_placeholder_meaning := to_meaning (f (line, 2));

									elsif kw = et_text.keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

-- 									elsif kw = et_text.keyword_line_width then -- line_width 0.2
-- 										expect_field_count (line, 2);
-- 										symbol_text_base.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := et_symbols.pac_text_schematic.to_rotation_doc (f (line, 2));

									elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := et_text.to_alignment (line, 2);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_PORTS =>
						case stack.parent is 
							when SEC_SYMBOL => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the port position starting at field 2
										port.position := to_position (line,2);

									elsif kw = keyword_name then -- name I1A
										expect_field_count (line, 2);
										port_name := to_port_name (f (line, 2));

									elsif kw = keyword_length then -- length 5
										expect_field_count (line, 2);
										port.length := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										port.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_port_name_visible then -- port_name_visible yes/no
										expect_field_count (line, 2);
										port.port_name_visible := to_port_name_visible (f (line, 2));

									elsif kw = keyword_port_name_size then -- port_name_size 2.0
										expect_field_count (line, 2);
										port.port_name_size := to_distance (f (line, 2));

									elsif kw = keyword_terminal_name_visible then -- terminal_name_visible yes/no
										expect_field_count (line, 2);
										port.terminal_name_visible := to_terminal_name_visible (f (line, 2));

									elsif kw = keyword_terminal_name_size then -- terminal_name_size 2.0
										expect_field_count (line, 2);
										port.terminal_name_size := to_distance (f (line, 2));

									elsif kw = et_symbols.keyword_direction then -- direction BIDIR, PASSIVE, NOT_CONNECTED, ...
										expect_field_count (line, 2);
										port_direction := to_port_direction (f (line, 2));

									elsif kw = keyword_sensitivity_edge then -- sensitivity_edge rising/falling/any
										expect_field_count (line, 2);
										port_sensitivity_edge := to_sensitivity_edge (f (line, 2));

									elsif kw = keyword_sensitivity_level then -- sensitivity_level high/low
										expect_field_count (line, 2);
										port_sensitivity_level := to_sensitivity_level (f (line, 2));

									elsif kw = keyword_inverted then -- inverted yes/no
										expect_field_count (line, 2);
										port_output_inverted := to_output_inverted (f (line, 2));

									elsif kw = keyword_tristate then -- tristate yes/no
										expect_field_count (line, 2);
										port_output_tristate := to_output_tristate (f (line, 2));

									elsif kw = keyword_level then -- level positive/negative/zero
										expect_field_count (line, 2);
										port_power_level := to_power_level (f (line, 2));

									elsif kw = keyword_weakness then -- weakness none/pull0/weak1 ...
										expect_field_count (line, 2);
										port_output_weakness := to_output_weakness (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & to_string (file_name) & space 
					 & affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;

		previous_input : ada.text_io.file_type renames current_input;
		
	begin -- read_device_file
		log_indentation_up;
		log (text => "reading device model " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- test if container devices already contains a model
		-- named "file_name". If so, there would be no need to read the file_name again.
		if pac_devices_lib.contains (devices, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			-- If the model file is to be read, first check if the file exists.
			declare
				file : string := et_general.expand (to_string (file_name));
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
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

			-- read the file line by line
			while not end_of_file loop
				line := read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Assemble final device and insert it in devices:
			case appearance is
				when PCB => -- a real device

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

					pac_devices_lib.insert (
						container	=> devices, 
						key			=> file_name, -- libraries/devices/7400.dev
						new_item	=> (
								appearance		=> PCB,
								prefix			=> prefix, -- IC
								units_internal	=> units_internal,
								units_external	=> units_external,
								value			=> value,
								--partcode		=> partcode,
								variants		=> variants));

				when VIRTUAL =>
					pac_devices_lib.insert (
						container	=> devices, 
						key			=> file_name, -- libraries/devices/power_gnd.dev
						new_item	=> (
								appearance		=> VIRTUAL,
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
	
end et_device_rw;

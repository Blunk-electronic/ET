------------------------------------------------------------------------------
--                                                                          --
--                        SYSTEM ET CONFIGURATION                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.strings; 				use ada.strings;
--with ada.strings.maps;			use ada.strings.maps;

-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings.fixed; 		use ada.strings.fixed;


with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;


with ada.directories;

with et_general;
with et_coordinates;
with et_libraries;
with et_schematic;
with et_string_processing;		use et_string_processing;
with et_export;
with et_import;

package body et_configuration is

	function to_submodule (abbrevation : in et_coordinates.type_submodule_abbrevation.bounded_string) 
		return type_import_module is
	-- Looks up the container import_modules for the given abbrevation and returns the submodule.
	-- Raises alarm if no submodule could be found -> abbrevation invalid.	
		use et_coordinates.type_submodule_abbrevation;
		use type_import_modules;
		module_cursor : type_import_modules.cursor := import_modules.first;
	begin
		while module_cursor /= type_import_modules.no_element loop
			if element (module_cursor).abbrevation = abbrevation then
				return element (module_cursor);
			end if;

			next (module_cursor);
		end loop;

		-- search without success:
		log_indentation_reset;
		log (message_error & "module abbrevation " & to_string (abbrevation) & " invalid !", console => true);
		raise constraint_error;
	end to_submodule;

	procedure check_multiple_purpose (
	-- Tests current module (indicated by module_cursor) if the given purpose is used
	-- only ONCE for the given category.
	-- Example: It is forbidden to have a two or more connectors with purpose "PWR_IN".
		category : in type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
		purpose : in et_libraries.type_component_purpose.bounded_string) is -- PWR_IN, SYS_FAIL, ...
	begin
		null; -- CS
	end check_multiple_purpose;
	
		
	procedure validate_module_interconnection (connection : in type_module_interconnection) is
	-- checks if something like "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN" makes sense
	-- in connection with entries in section import_modules
		use et_coordinates;
		module_A, module_B : type_import_module;

		procedure instance_invalid (
			name : type_submodule_name.bounded_string;
			instance_is, instance_max : type_submodule_instance) is
		begin
			log_indentation_reset;
			log (message_error & "instance index " & to_string (instance_is) 
				& " for submodule " & to_string (name) & " invalid !", console => true);
			log ("Max number of instances specified in section " & section_import_modules & " is " 
				 & to_string (instance_max) & " !", console => true);
			raise constraint_error;
		end instance_invalid;
		
	begin --validate_module_interconnection
		-- load module A/B from the given abbrevation.
		-- Test if given abbrevation is in range of total number of instances for the module.
		module_A := to_submodule (connection.peer_A.abbrevation); -- reason from NCC to "nucleo_core NCC kicad_v4 1"
		if connection.peer_A.instance > module_A.instances then -- instance index check
			instance_invalid (module_A.name, connection.peer_A.instance, module_A.instances);
		end if;
		
		module_B := to_submodule (connection.peer_B.abbrevation); -- reason from MOT to "motor_driver MOT kicad_v4 2"
		if connection.peer_B.instance > module_B.instances then -- instance index check
			instance_invalid (module_B.name, connection.peer_B.instance, module_B.instances);
		end if;
	end validate_module_interconnection;

	function to_component_reference (
	-- Returns the reference (like X4) of the connector (in the given module) with the given purpose.
		module_name : in et_coordinates.type_submodule_name.bounded_string;
		connector_purpose : in et_libraries.type_component_purpose.bounded_string) 
		return et_libraries.type_component_reference is

		use et_schematic;
		use type_rig;
		module_cursor : type_rig.cursor;
		ref : et_libraries.type_component_reference;
	begin
		-- locate the module in the rig
		module_cursor := find (rig, module_name);
-- 		if module_cursor = no_element then
-- 			log_indentation_reset;
-- 			log (message_error & "
		
		return ref;
	end to_component_reference;
	
	procedure validate_module_interconnections (log_threshold: in et_string_processing.type_log_level) is
	-- Tests if module interconnections at net level make sense.
	-- NOTE: call AFTER modules have been imported !
		interconnection_cursor : type_module_interconnections.cursor := module_interconnections.first;
		reference_A, reference_B : et_libraries.type_component_reference;
	begin
		log ("validating module interconnections ...", log_threshold + 1);
		-- From the module name (nucleo_core) and the purpose (MOTOR_CTRL_OUT_2) of the connector
		-- we reason the references like X1, X46
-- 		reference_A := to_component_reference (module_A.name, connection.peer_A.purpose);
-- 		reference_B := to_component_reference (module_B.name, connection.peer_B.purpose);		

	
		null;
	end validate_module_interconnections;
	
	function to_string (cat : in type_component_category) return string is
	-- returns the given component category as string
	begin
		return latin_1.space & type_component_category'image (cat);
	end to_string;

	function to_category (category : in string) return type_component_category is
	-- Converts a string to type_component_category.
		use et_string_processing;
		category_out : type_component_category;
	begin
		category_out := type_component_category'value (category);
		return category_out;

		exception
			when others =>
				log_indentation_reset;
				log (message_error & category & " is not a supported component category !",
					 console => true);

				-- CS: show supported categories
				
				raise constraint_error;
	end to_category;
	
	function category (prefix : in et_libraries.type_component_prefix.bounded_string) return
		type_component_category is
	-- Returns the category of the given component prefix. If no category could be
	-- found, returns category UNKNOWN.
		use et_libraries.type_component_prefix;
		use type_component_prefixes;

		prefix_cursor : type_component_prefixes.cursor;
	begin
		-- locate prefix as specified by configuration file
		prefix_cursor := component_prefixes.find (prefix);

		-- If prefix not specified (or no configuration at all) return category UNKNOWN.
		-- Otherwise return the respecitve category.
		if prefix_cursor = type_component_prefixes.no_element then
			log (message_warning & " category of prefix " 
				 & et_libraries.to_string (prefix)
				 & latin_1.space
				 & to_string (UNKNOWN) & " !");
			return UNKNOWN;
		else
			return element (prefix_cursor);
		end if;
	end category;

	
	function category (reference : in et_libraries.type_component_reference) return
		type_component_category is
	-- Returns the category of the given component reference. If no category could be
	-- found, returns category UNKNOWN.
		use et_libraries.type_component_prefix;
		use type_component_prefixes;

		prefix_cursor : type_component_prefixes.cursor;
	begin
		-- locate prefix as specified by configuration file
		prefix_cursor := component_prefixes.find (reference.prefix);

		-- If prefix not specified (or no configuration at all) return category UNKNOWN.
		-- Otherwise return the respecitve category.
		if prefix_cursor = type_component_prefixes.no_element then
			log (message_warning & " category of component " 
				 & et_libraries.to_string (reference)
				 & to_string (UNKNOWN) & " !");
			return UNKNOWN;
		else
			return element (prefix_cursor);
		end if;
	end category;

	function to_unit_of_measurement (unit : in string) return type_unit_of_measurement is
	-- Converts a string to type_component_unit_meaning.
		use et_string_processing;
		unit_out : type_unit_of_measurement;
	begin
		unit_out := type_unit_of_measurement'value (unit);
		return unit_out;

		exception
			when others =>
				log_indentation_reset;
				log (message_error & unit & " is not a supported unit of measurement !",
					 console => true);

				-- CS: show supported units
				
				raise constraint_error;
	end to_unit_of_measurement;
	
	function to_string (unit : in type_unit_of_measurement) return string is
	-- returns the given unit of measurement as string. (things like OHM, KILOOHM, MEGAOHM, ...)
	begin
		return latin_1.space & type_unit_of_measurement'image (unit);
	end to_string;
	
	procedure check_abbrevation_of_unit_characters (
		abbrevation : in type_unit_abbrevation.bounded_string;
		characters : in character_set) is
	-- Tests if the given abbrevation contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
		use et_string_processing;
		use type_unit_abbrevation;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => abbrevation,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "abbrevaton of unit of measurement " 
				& to_string (abbrevation) 
				& " has invalid character at position"
				& natural'image (invalid_character_position),
				console => true);
			raise constraint_error;
		end if;
	end check_abbrevation_of_unit_characters;

	function requires_operator_interaction (
		prefix : in et_libraries.type_component_prefix.bounded_string) 
		return type_component_requires_operator_interaction is
	-- Returns YES is given prefix requires operator interaction.
		cat : type_component_category;
		use type_categories_with_operator_interacton;		
		cat_cursor : type_categories_with_operator_interacton.cursor;
	begin
		-- get category from given prefix
		cat := category (prefix);

		-- search in container component_categories_with_operator_interaction for
		-- category cat.
		cat_cursor := component_categories_with_operator_interaction.find (cat);

		if cat_cursor = no_element then
			return NO; -- no operator interaction required
		else
			return YES; -- operator interaction required
		end if;
	end requires_operator_interaction;
		
	procedure make_default_configuration (
		file_name		: in type_configuration_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	-- Creates a default configuration file.
		use et_general;

		function comment return string is begin return comment_mark & latin_1.space; end comment;

	begin
		et_export.create_report;
		reset_warnings_counter;

	
		log ("generating default configuration file " & to_string (file_name), log_threshold);

		if exists (to_string (file_name)) then
			-- CS: warn operator and request confirmation
			null;
		end if;
		
		create (
			file => configuration_file_handle, 
			mode => out_file, 
			name => to_string (file_name));

		put_line (configuration_file_handle, comment & system_name & " configuration");
		put_line (configuration_file_handle, comment & "auto generated by "
			& system_name & latin_1.space & version & " at date " & string (date_now));
		put_line (configuration_file_handle, comment & "Please modify it according to your needs.");
		put_line (configuration_file_handle, comment & row_separator_double);
		new_line (configuration_file_handle);

		-- MODULES TO BE IMPORTED
		put_line (configuration_file_handle, section_import_modules); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "module abbrevation cad_format [number_of_instances]");
		put_line (configuration_file_handle, comment & "examples:");
		put_line (configuration_file_handle, comment & "nucleo_core NCC kicad_v4 1");
		put_line (configuration_file_handle, comment & "motor_driver MOT kicad_v4 2");
		new_line (configuration_file_handle);		

		-- MODULE INTERCONNECTIONS
		put_line (configuration_file_handle, section_module_interconnections); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "abbrevation instance connector_purpose abbrevation instance connector_purpose");
		put_line (configuration_file_handle, comment & "examples:");
		put_line (configuration_file_handle, comment & "NCC 1 MOTOR_CTRL_OUT_1 MOT 1 MOTOR_CTRL_IN");
		put_line (configuration_file_handle, comment & "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN");
		new_line (configuration_file_handle);		
		
		-- COMPONENT PREFIXES
		put_line (configuration_file_handle, section_component_prefixes); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "prefix category");
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, "ANT" & to_string (ANTENNA)); -- CS: short desciption as comment for all
		put_line (configuration_file_handle, "B  " & to_string (BUZZER));
		put_line (configuration_file_handle, "BAT" & to_string (BATTERY));
		put_line (configuration_file_handle, "C  " & to_string (CAPACITOR));
		put_line (configuration_file_handle, "CA " & to_string (CAPACITOR_ADJUSTABLE));
		--put_line (configuration_file_handle, "CBL" & to_string (CABLE));
		put_line (configuration_file_handle, "D  " & to_string (DIODE));
		put_line (configuration_file_handle, "DPH" & to_string (DIODE_PHOTO));
		put_line (configuration_file_handle, "DI " & to_string (DIAC));
		put_line (configuration_file_handle, "DIS" & to_string (DISPLAY));
		put_line (configuration_file_handle, "F  " & to_string (FUSE));
		put_line (configuration_file_handle, "HS " & to_string (HEATSINK));
		put_line (configuration_file_handle, "IC " & to_string (INTEGRATED_CIRCUIT));
		put_line (configuration_file_handle, "J  " & to_string (JUMPER));
		put_line (configuration_file_handle, "JD " & to_string (JUMPER));
		put_line (configuration_file_handle, "K  " & to_string (RELAY));
		put_line (configuration_file_handle, "KP " & to_string (KEYPAD));
		put_line (configuration_file_handle, "L  " & to_string (INDUCTOR));
		put_line (configuration_file_handle, "LS " & to_string (LOUDSPEAKER));
		put_line (configuration_file_handle, "LED" & to_string (LIGHT_EMMITTING_DIODE));
		put_line (configuration_file_handle, "M  " & to_string (MOTOR));
		put_line (configuration_file_handle, "MIC" & to_string (MICROPHONE));
		put_line (configuration_file_handle, "N  " & to_string (NETCHANGER));
		put_line (configuration_file_handle, "OC " & to_string (OPTOCOUPLER));		
		put_line (configuration_file_handle, "Q  " & to_string (QUARTZ));
		put_line (configuration_file_handle, "R  " & to_string (RESISTOR));
		put_line (configuration_file_handle, "RA " & to_string (RESISTOR_ADJUSTABLE));
		put_line (configuration_file_handle, "RN " & to_string (RESISTOR_NETWORK));
		put_line (configuration_file_handle, "RP " & to_string (POTENTIOMETER));
		put_line (configuration_file_handle, "RPH" & to_string (RESISTOR_PHOTO));
		put_line (configuration_file_handle, "S  " & to_string (SWITCH));
		put_line (configuration_file_handle, "T  " & to_string (TRANSISTOR));
		put_line (configuration_file_handle, "TP " & to_string (TRANSISTOR_PHOTO));
		put_line (configuration_file_handle, "TF " & to_string (TRANSFORMER));
		put_line (configuration_file_handle, "TPT" & to_string (TESTPOINT));
		put_line (configuration_file_handle, "TH " & to_string (THYRISTOR));
		put_line (configuration_file_handle, "THP" & to_string (THYRISTOR_PHOTO));
		put_line (configuration_file_handle, "TR " & to_string (TRIAC));
		put_line (configuration_file_handle, "TUB" & to_string (TUBE));		
		--put_line (configuration_file_handle, "W" & to_string (WIRE));
		put_line (configuration_file_handle, "X  " & to_string (CONNECTOR));
		put_line (configuration_file_handle, "XD " & to_string (CONNECTOR));
		
		new_line (configuration_file_handle);
		new_line (configuration_file_handle);		

		-- UNITS OF COMPONENT VALUES
		put_line (configuration_file_handle, section_component_units); -- section header
		new_line (configuration_file_handle);
		put_line (configuration_file_handle, comment & "abbrevation unit_of_measurement");
		new_line (configuration_file_handle);
		put_line (configuration_file_handle, "m" & to_string (MILLIOHM));
		put_line (configuration_file_handle, "R" & to_string (OHM));
		put_line (configuration_file_handle, "k" & to_string (KILOOHM));
		put_line (configuration_file_handle, "M" & to_string (MEGAOHM));
		put_line (configuration_file_handle, "G" & to_string (GIGAOHM));		

		put_line (configuration_file_handle, "p" & to_string (PICOFARAD));		
		put_line (configuration_file_handle, "n" & to_string (NANOFARAD));
		put_line (configuration_file_handle, "u" & to_string (MICROFARAD));
		put_line (configuration_file_handle, "m" & to_string (MILLIFARAD));
		put_line (configuration_file_handle, "F" & to_string (FARAD));

		put_line (configuration_file_handle, "n" & to_string (NANOHENRY));		
		put_line (configuration_file_handle, "u" & to_string (MICROHENRY));
		put_line (configuration_file_handle, "m" & to_string (MILLIHENRY));
		put_line (configuration_file_handle, "H" & to_string (HENRY));

		put_line (configuration_file_handle, "V" & to_string (VOLT));

		put_line (configuration_file_handle, "m" & to_string (MILLIAMPERE));		
		put_line (configuration_file_handle, "A" & to_string (AMPERE));

		put_line (configuration_file_handle, "k" & to_string (KILOHERTZ));
		put_line (configuration_file_handle, "M" & to_string (MEGAHERTZ));
		put_line (configuration_file_handle, "G" & to_string (GIGAHERTZ));
		
		new_line (configuration_file_handle);
		new_line (configuration_file_handle);		

		-- COMPONENT THAT REQUIRE OPERATOR INTERACTION
		put_line (configuration_file_handle, section_components_with_operator_interaction); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "category");
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, to_string (BUZZER));
		put_line (configuration_file_handle, to_string (CAPACITOR_ADJUSTABLE));
		put_line (configuration_file_handle, to_string (CONNECTOR));
		put_line (configuration_file_handle, to_string (DIODE_PHOTO));
		put_line (configuration_file_handle, to_string (DISPLAY));
		put_line (configuration_file_handle, to_string (FUSE));
		put_line (configuration_file_handle, to_string (JUMPER));
		put_line (configuration_file_handle, to_string (KEYPAD));
		put_line (configuration_file_handle, to_string (LIGHT_EMMITTING_DIODE));
		put_line (configuration_file_handle, to_string (RESISTOR_ADJUSTABLE));
		put_line (configuration_file_handle, to_string (RESISTOR_PHOTO));
		put_line (configuration_file_handle, to_string (POTENTIOMETER));
		put_line (configuration_file_handle, to_string (SWITCH));
		put_line (configuration_file_handle, to_string (TESTPOINT));
		put_line (configuration_file_handle, to_string (THYRISTOR_PHOTO));

		new_line (configuration_file_handle);
		

		-- CONNECTOR GND TERMINAL
        -- CS
-- 		put_line (configuration_file_handle, section_connector_gnd_terminal); -- section header
-- 		new_line (configuration_file_handle);

		-- CS LINE WIDTHS

		-- CS TEXT SIZES
	
		
		put_line (configuration_file_handle, comment & row_separator_double);		
		put_line (configuration_file_handle, comment & system_name & " configuration end");
		close (configuration_file_handle);

		et_export.close_report;

		exception
			when event:
				others => 
					et_export.close_report;
					put_line (standard_output, message_error & "Read export report for warnings and error messages !"); -- CS: show path to report file

		
	end make_default_configuration;


	procedure read_configuration (
		file_name		: in type_configuration_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	-- Reads the given configuration file.
	-- Fills component_prefixes.

		use et_string_processing;
		line : et_string_processing.type_fields_of_line; -- the line of the file

		function field (line : in type_fields_of_line; position : in positive) return string renames
			et_string_processing.get_field_from_line;

		type type_section is (
			none,
			import_modules,
			module_interconnections,
			component_prefixes,
			component_units,
			components_with_operator_interaction
			);
		
		section_entered : type_section := none;
		
		-- lines of the file are collected in a simple list:
		package type_lines is new doubly_linked_lists (
			element_type => et_string_processing.type_fields_of_line,
			"=" => et_string_processing.lines_equally);
		use type_lines;
		lines : type_lines.list := type_lines.empty_list;
			
		procedure process_previous_section is
		-- Processes the section indicated by section_entered. 
		-- The lines of the section are in container "lines".
		-- Clears "lines" after processing.
			line_cursor : type_lines.cursor := lines.first; -- points to the line being processed
			component_prefix_cursor : type_component_prefixes.cursor; -- CS: rename to prefix_cursor
			unit_cursor : type_units_of_measurement.cursor;
			inserted : boolean := false;

			-- we deal with columns and need to index them
			subtype type_column is positive range 1..6;
			column_module_name	: type_column;
			column_cad_format	: type_column;
			column_abbrevation_A,	column_abbrevation_B	: type_column;
			column_instance_A, 		column_instance_B		: type_column;		
			column_purpose_A,		column_purpose_B		: type_column;
					
			use et_libraries;
			use et_coordinates;
			use type_import_modules;

			procedure test_multiple_occurences is 
			begin
				if not inserted then
					log (message_warning & affected_line (element (line_cursor)) & "multiple occurence of assignment ! Entry ignored !");
				end if;
			end test_multiple_occurences;

			procedure test_multiple_occurences (module : in type_import_module) is
			-- Tests if given module is already in the import_modules list.
				module_cursor : type_import_modules.cursor := et_configuration.import_modules.first;
				use type_submodule_name;
			begin
				while module_cursor /= type_import_modules.no_element loop
					if element (module_cursor).name = module.name then
						log_indentation_reset;
						log (message_error & affected_line (element (line_cursor)) 
								& "duplicated entry !",
								console => true);
						raise constraint_error;
					end if;
					next (module_cursor);
				end loop;
			end test_multiple_occurences;

			procedure test_multiple_occurences (connection : in type_module_interconnection) is
			-- Tests if given module inteconnection is already in the module_interconnections list.
				use type_module_interconnections;
			begin
				if find (et_configuration.module_interconnections, connection) /= 
					type_module_interconnections.no_element then
						log_indentation_reset;
						log (message_error & affected_line (element (line_cursor)) 
							& "duplicated entry !",
							console => true);
						raise constraint_error;
				end if;
			end test_multiple_occurences;

			
			module		: type_import_module;
			connection	: type_module_interconnection;
			
			prefix 		: type_component_prefix.bounded_string;
			cat 		: type_component_category;
			
			abbrevation	: type_unit_abbrevation.bounded_string;
			unit		: type_unit_of_measurement;
			
			-- CS: check field count in sections respectively. issue warning if too many fields. 
		begin
			next (line_cursor); -- the first line of the section is its header itself and can be skipped
			log_indentation_up;

			case section_entered is
				when none => null;

				-- MODULES TO BE IMPORTED
				when import_modules =>
					log ("import modules ...", log_threshold + 1);
					log_indentation_up;

					-- Mind table header in conf. file when changing anything here.
					-- See procedure make_default_configuration.
					column_module_name		:= 1; 
					column_abbrevation_A	:= 2;
					column_cad_format		:= 3;
					column_instance_A		:= 4;
					
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- read module name
						check_submodule_name_length (field (element (line_cursor), column_module_name));
						module.name := to_submodule_name (field (element (line_cursor), column_module_name));
						check_submodule_name_characters (module.name);
						et_import.validate_project (et_schematic.to_project_name (field (element (line_cursor), column_module_name)));

						-- read abbrevation
						check_submodule_abbrevation_length (field (element (line_cursor), column_abbrevation_A));
						module.abbrevation := to_abbrevation (field (element (line_cursor), column_abbrevation_A));
						check_submodule_abbrevation_characters (module.abbrevation);

						-- read cad format
						et_import.validate_cad_format (field (element (line_cursor), column_cad_format));
						module.format := et_import.to_cad_format (field (element (line_cursor), column_cad_format));
						-- CS: default if not provided ?

						-- read number of instances
						check_number_of_instances (field (element (line_cursor), column_instance_A)); -- character check included
						module.instances := to_number_of_instances (field (element (line_cursor), column_instance_A));
						-- CS: default if not provided ?

						-- test multiple occurences of module name
						test_multiple_occurences (module);

						-- insert module in container
						type_import_modules.append (
							container => et_configuration.import_modules,
							new_item => module);
						
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- MODULE INTERCONNECTIONS
				when module_interconnections =>
					log ("module interconnections ...", log_threshold + 1);
					log_indentation_up;

					-- Mind table header in conf. file when changing anything here.
					-- See procedure make_default_configuration.
					column_abbrevation_A	:= 1;
					column_instance_A		:= 2;
					column_purpose_A		:= 3;
					column_abbrevation_B	:= 4;
					column_instance_B		:= 5;
					column_purpose_B		:= 6;

					-- we read a line like "# NCC 1 MOTOR_CTRL_OUT_1 MOT 1 MOTOR_CTRL_IN"
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- read module abbrevation A
						check_submodule_abbrevation_length (field (element (line_cursor), column_abbrevation_A));
						connection.peer_A.abbrevation := to_abbrevation (field (element (line_cursor), column_abbrevation_A));
						check_submodule_abbrevation_characters (connection.peer_A.abbrevation);

						-- read number of instances
						check_number_of_instances (field (element (line_cursor), column_instance_A)); -- character check included
						connection.peer_A.instance := to_number_of_instances (field (element (line_cursor), column_instance_A));
						
						-- read connector purpose A
						check_purpose_length (field (element (line_cursor), column_purpose_A));
						validate_purpose (field (element (line_cursor), column_purpose_A));
						connection.peer_A.purpose := to_purpose (field (element (line_cursor), column_purpose_A));
						check_purpose_characters (connection.peer_A.purpose);
						-- NOTE: The question whether there is a connector with this purpose in the module can
						-- not be answered here, because the project has not been imported yet.
						
						-- read module abbrevation B
						check_submodule_abbrevation_length (field (element (line_cursor), column_abbrevation_B));
						connection.peer_B.abbrevation := to_abbrevation (field (element (line_cursor), column_abbrevation_B));
						check_submodule_abbrevation_characters (connection.peer_B.abbrevation);

						-- read number of instances
						check_number_of_instances (field (element (line_cursor), column_instance_B)); -- character check included
						connection.peer_B.instance := to_number_of_instances (field (element (line_cursor), column_instance_B));
						
						-- read connector purpose B
						check_purpose_length (field (element (line_cursor), column_purpose_B));
						validate_purpose (field (element (line_cursor), column_purpose_B));
						connection.peer_B.purpose := to_purpose (field (element (line_cursor), column_purpose_B));
						check_purpose_characters (connection.peer_B.purpose);
						-- NOTE: The question whether there is a connector with this purpose in the module can
						-- not be answered here, because the project has not been imported yet.
						
						-- test multiple occurences of connection
						test_multiple_occurences (connection);

						-- check if all this makes sense in connection with entries in section import_modules
						validate_module_interconnection (connection);
						
						-- insert interconnection in container
						type_module_interconnections.append (
							container => et_configuration.module_interconnections,
							new_item => connection);
						
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- COMPONENT PREFIXES
				when component_prefixes =>
					log ("component prefixes ...", log_threshold + 1);
					log_indentation_up;

					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- Build the prefix from field #1:
						-- Test if prefix is not too long, if it contains only allowed characters.
						-- We test against the default character set as specified in et_libraries.
						check_prefix_length (field (element (line_cursor), 1));
						prefix := type_component_prefix.to_bounded_string (field (element (line_cursor), 1));
						check_prefix_characters (prefix, component_prefix_characters);

						-- build the component category from field #2:
						cat := to_category (field (element (line_cursor), 2));
						
						-- insert the prefix assignment in container component_prefixes
						type_component_prefixes.insert (
							container => et_configuration.component_prefixes,
							position => component_prefix_cursor,
							key => prefix,
							new_item => cat,
							
							-- If entry already in map, this flag goes true. Warning issued later. see below.
							inserted => inserted);

						test_multiple_occurences;
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- COMPONENT UNITS OF MEASUREMENT
				when component_units =>
					log ("component units of measurement ...", log_threshold + 1);
					log_indentation_up;
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- Build the unit abbrevation from field #1:
						-- Test if abbrevation contains only allowed characters.
						-- We test against the character set specified for abbrevations of units of measurement.
						abbrevation := type_unit_abbrevation.to_bounded_string (field (element (line_cursor), 1));
						check_abbrevation_of_unit_characters (abbrevation, unit_abbrevation_characters);

						-- Build the unit of measurement from field #2:
						unit := to_unit_of_measurement (field (element (line_cursor), 2));
						
						-- insert the abbrevation to unit of measurement assignment in container component_units
						type_units_of_measurement.insert (
							container => et_configuration.component_units,
							position => unit_cursor,

							-- If entry already in map, this flag goes true. Warning issued later. see below.
							inserted => inserted,

							-- the key in this map is the unit of measurement like MICROFARAD or NANOHENRY
							key => unit,
							
							-- the item is the abbrevation of unit of measurement
							new_item => abbrevation
							);

						test_multiple_occurences;
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- COMPONENTS WITH USER INTERACTON
				when components_with_operator_interaction =>
					log ("component categories with operator interaction ...", log_threshold + 1);
					log_indentation_up;
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- build the component category from field #1:
						cat := to_category (field (element (line_cursor), 1));

						-- insert the category in container component_categories_with_operator_interaction
						type_categories_with_operator_interacton.insert (
							container => component_categories_with_operator_interaction,
							new_item => cat);
						
						next (line_cursor);
					end loop;
					log_indentation_down;

					
			end case;
			
			log_indentation_down;

			-- clean up. empty container "lines" for next section
			lines.clear;

			exception
				when others =>
					log_indentation_reset;
					log (message_error & affected_line (element (line_cursor)) & latin_1.space & to_string (element (line_cursor)),
						 console => true);

					-- CS: provide information on what is wrong with the line (depending on section_entered)
					-- number of characters in unit of measurement
					-- propose category, units, ...
					
					raise;
					
		end process_previous_section;
		
	begin -- read_configuration
		log ("reading configuration file " & to_string (file_name) & " ...", log_threshold);

		if exists (to_string (file_name)) then

			et_configuration.component_prefixes := type_component_prefixes.empty_map;

			open (file => configuration_file_handle, mode => in_file, name => to_string (file_name));
			set_input (configuration_file_handle);

			-- read configuration file line per line
			while not end_of_file loop

				-- Store line in variable "line" (see et_string_processing.ads)
				line := et_string_processing.read_line (
					line => get_line,
					number => ada.text_io.line (current_input),
					comment_mark => et_general.comment_mark,
					-- CS delimiter_wrap => true, -- if connector purpose is given in quotations
					ifs => latin_1.space); -- fields are separated by space

				case field_count (line) is
					when 0 => null; -- we skip empty lines
					when others =>

						-- CS: check field count ?
						
						-- At a certain log level we report the whole line as it is:
						--log (to_string (line), log_threshold + 3);

						-- Wait for a section header.
						-- Once a header was found, the PREVIOUS section is regarded as complete.
						-- The PREVIOUS section is then processed with all its lines in container "lines".
						-- Set section_entered according to the section BEING entered.
						if field (line, 1) = section_import_modules then
							process_previous_section;
							section_entered := import_modules;
						end if;

						if field (line, 1) = section_module_interconnections then
							process_previous_section;
							section_entered := module_interconnections;
						end if;
						
						if field (line, 1) = section_component_prefixes then
							process_previous_section;
							section_entered := component_prefixes;
						end if;

						if field (line, 1) = section_component_units then
							process_previous_section;
							section_entered := component_units;
						end if;

						if field (line, 1) = section_components_with_operator_interaction then
							process_previous_section;
							section_entered := components_with_operator_interaction;
						end if;

						-- CS: place other sections header tests here
						
						-- For all entered sections collect lines in container "lines".
						case section_entered is
							when none => null;
							when others =>
								lines.append (line);
						end case;
						
				end case;
				
			end loop;

			-- The last section of the file is complete, once the file end is reached.
			process_previous_section;
			
			set_input (standard_input);
			close (configuration_file_handle);
			
		else
			log_indentation_reset;
			log (message_error & "configuration file " & to_string (file_name) & " not found !",
				 console => true);
			raise constraint_error;
		end if;

	end read_configuration;

	procedure validate_prefix (prefix : in et_libraries.type_component_prefix.bounded_string) is
	-- Tests if the given prefix is a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use et_libraries.type_component_prefix;
		use type_component_prefixes;
	
		prefix_cursor : type_component_prefixes.cursor;
	begin
		if component_prefixes.find (prefix) /= type_component_prefixes.no_element then
			null;
		else
			log_indentation_reset;
			log (message_error & "invalid prefix "
				 & to_string (prefix) & " !"
				 & " See configuration file for valid prefixes.",
				console => true
				);
			raise constraint_error;
		end if;
	end validate_prefix;
	
	procedure validate_prefix (reference : in et_libraries.type_component_reference) is
	-- Tests if the given reference has a valid prefix as specified in the configuration file.
	-- Raises exception if not.
		use et_libraries.type_component_prefix;
		use type_component_prefixes;
	begin
		if component_prefixes.find (reference.prefix) /= type_component_prefixes.no_element then
			null;
		else
			log_indentation_reset;
			log (message_error & "invalid prefix in component reference "
				 & et_libraries.to_string (reference) & " !"
				 & " See configuration file for valid prefixes.",
				console => true
				);
			-- CS: show coordinates of affected component
			raise constraint_error;
		end if;
	end validate_prefix;

	
end et_configuration;

-- Soli Deo Gloria

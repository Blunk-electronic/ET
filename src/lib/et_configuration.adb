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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings; 				use ada.strings;
-- with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;


with ada.directories;

with et_general;
with et_libraries;
with et_string_processing;		use et_string_processing;
with et_export;

package body et_configuration is

	function to_string (cat : in type_component_category) return string is
	-- returns the given component category as string
	begin
		return latin_1.space & type_component_category'image (cat);
	end to_string;

	function to_string (unit : in type_component_value) return string is
	-- returns the given component value as string
	begin
		return latin_1.space & type_component_value'image (unit);
	end to_string;

	
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
		put_line (configuration_file_handle, comment & "auto generated at date " & string (date_now));
		put_line (configuration_file_handle, comment & "Please modify it according to your needs.");
		put_line (configuration_file_handle, comment & row_separator_double);
		new_line (configuration_file_handle);

		-- COMPONENT PREFIXES
		put_line (configuration_file_handle, section_component_prefixes); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "prefix category");
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, "R  " & to_string (RESISTOR));
		put_line (configuration_file_handle, "C  " & to_string (CAPACITOR));
		put_line (configuration_file_handle, "L  " & to_string (INDUCTOR));
		put_line (configuration_file_handle, "TF " & to_string (TRANSFORMER));
		put_line (configuration_file_handle, "D  " & to_string (DIODE));
		put_line (configuration_file_handle, "T  " & to_string (TRANSISTOR));
		put_line (configuration_file_handle, "LED" & to_string (LIGHT_EMMITTING_DIODE));
		put_line (configuration_file_handle, "IC " & to_string (INTEGRATED_CIRCUIT));
		put_line (configuration_file_handle, "N  " & to_string (NETCHANGER));
		--put_line (configuration_file_handle, "W" & to_string (WIRE));
		--put_line (configuration_file_handle, "CBL" & to_string (CABLE));
		put_line (configuration_file_handle, "X  " & to_string (CONNECTOR));
		put_line (configuration_file_handle, "J  " & to_string (JUMPER));
		put_line (configuration_file_handle, "S  " & to_string (SWITCH));
		put_line (configuration_file_handle, "K  " & to_string (RELAY));
		put_line (configuration_file_handle, "M  " & to_string (MOTOR));
		put_line (configuration_file_handle, "B  " & to_string (BUZZER));
		put_line (configuration_file_handle, "LS " & to_string (LOUDSPEAKER));
		put_line (configuration_file_handle, "MIC" & to_string (MICROPHONE));
		
		new_line (configuration_file_handle);
		new_line (configuration_file_handle);		

		-- UNITS OF COMPONENT VALUES
		put_line (configuration_file_handle, section_component_values); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, "R" & to_string (OHM));
		put_line (configuration_file_handle, "m" & to_string (MILLIOHM));
		put_line (configuration_file_handle, "k" & to_string (KILOOHM));
		put_line (configuration_file_handle, "M" & to_string (MEGAOHM));

		put_line (configuration_file_handle, "F" & to_string (FARAD));
		put_line (configuration_file_handle, "m" & to_string (MILLIFARAD));
		put_line (configuration_file_handle, "u" & to_string (MICROFARAD));
		put_line (configuration_file_handle, "n" & to_string (NANOFARAD));
		put_line (configuration_file_handle, "p" & to_string (PICOFARAD));

		put_line (configuration_file_handle, "H" & to_string (HENRY));
		put_line (configuration_file_handle, "m" & to_string (MILLIHENRY));
		put_line (configuration_file_handle, "u" & to_string (MICROHENRY));
		put_line (configuration_file_handle, "n" & to_string (NANOHENRY));
		
		new_line (configuration_file_handle);
		new_line (configuration_file_handle);		

		-- COMPONENT WITH OPERATOR INTERACTION
		put_line (configuration_file_handle, section_components_with_operator_interaction); -- section header
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, comment & "prefix");
		new_line (configuration_file_handle);		
		put_line (configuration_file_handle, "LED");
		put_line (configuration_file_handle, "S");
		put_line (configuration_file_handle, "B");
		put_line (configuration_file_handle, "X");
		put_line (configuration_file_handle, "J");
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
			component_prefixes,
			component_values,
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
			component_prefix_cursor : type_configuration_component_prefixes.cursor;
			inserted : boolean := false;

			use et_libraries;

			-- CS: check field count in sections respecitvely. issue warning if too many fields. 
		begin
			next (line_cursor); -- the first line of the section is its header itself and can be skipped
			log_indentation_up;

			case section_entered is
				when none => null;

				-- COMPONENT PREFIXES
				when component_prefixes =>
					log ("component prefixes ...", log_threshold + 1);
					log_indentation_up;
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);

						-- insert the prefix assignment in container configuration_component_prefixes
						type_configuration_component_prefixes.insert (
							container => configuration_component_prefixes,
							position => component_prefix_cursor,

							-- If entry already in map, this flag goes true. Warning issued later. see below.
							inserted => inserted,
							
							-- Test if prefix contains only allowed characters. Then use it as key in this map.
							-- We test against the default character set as specified in et_libraries.
							key => check_prefix_characters (
								prefix => type_component_prefix.to_bounded_string (field (element (line_cursor), 1)),
								characters => component_prefix_characters),

							-- Test if component category is valid. Then use it as new item.
							new_item => type_component_category'value (field (element (line_cursor), 2)));

						
						if not inserted then
							log (message_warning & affected_line (element (line_cursor)) & "multiple occurence of assignment ! Entry ignored !");
						end if;
						
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- COMPONENT VALUES
				when component_values =>
					log ("component values ...", log_threshold + 1);
					log_indentation_up;
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);
						-- CS
						next (line_cursor);
					end loop;
					log_indentation_down;

				-- COMPONENTS WITH USER INTERACTON
				when components_with_operator_interaction =>
					log ("components with operator interaction ...", log_threshold + 1);
					log_indentation_up;
					while line_cursor /= type_lines.no_element loop
						log (to_string (element (line_cursor)), log_threshold + 2);
						-- CS
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
						 console => false);

					-- CS: provide information on what is wrong with the line (depending on section_entered)
					-- propose category, units, ...
					
					raise;
					
		end process_previous_section;
		
	begin -- read_configuration
		log ("reading configuration file " & to_string (file_name) & " ...", log_threshold);

		if exists (to_string (file_name)) then

			configuration_component_prefixes := type_configuration_component_prefixes.empty_map;

			open (file => configuration_file_handle, mode => in_file, name => to_string (file_name));
			set_input (configuration_file_handle);

			-- read configuration file line per line
			while not end_of_file loop

				-- Store line in variable "line" (see et_string_processing.ads)
				line := et_string_processing.read_line (
					line => get_line,
					number => ada.text_io.line (current_input),
					comment_mark => et_general.comment_mark,
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
						if field (line, 1) = section_component_prefixes then
							process_previous_section;
							section_entered := component_prefixes;
						end if;

						if field (line, 1) = section_component_values then
							process_previous_section;
							section_entered := component_values;
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
	
end et_configuration;

-- Soli Deo Gloria

------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD PCB                            --
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
with ada.characters.handling;	use ada.characters.handling;

with ada.directories;
with ada.strings;				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with ada.exceptions;

with et_general;
with et_libraries;
with et_pcb;
with et_pcb_coordinates;
with et_string_processing;		use et_string_processing;

with et_kicad;

package body et_kicad_pcb is

	function to_assembly_technology (tech : in string) return et_pcb.type_assembly_technology is
		use et_pcb;
	begin
		if tech = "smd" then return SMT;
		elsif tech = "thru_hole" then return THT;
		else
			log_indentation_reset;
			log (message_error & "invalid assembly technology", console => true);
			raise constraint_error;
		end if;
	end to_assembly_technology;
			
	function to_terminal_shape_tht (shape : in string) return et_pcb.type_terminal_shape_tht is
		use et_pcb;
	begin
		if shape = "rect" then return RECTANGLE;
		elsif shape = "oval" then return LONG;
		else
			log_indentation_reset;
			log (message_error & "invalid shape for a THT terminal !", console => true);
			raise constraint_error;
		end if;
	end to_terminal_shape_tht;

	function to_terminal_shape_smt (shape : in string) return et_pcb.type_terminal_shape_smt is
		use et_pcb;
	begin
		if shape = "rect" then return RECTANGLE;
		elsif shape = "oval" then return LONG;
		elsif shape = "circle" then return CIRCULAR;
		else
			log_indentation_reset;
			log (message_error & "invalid shape for an SMT terminal !", console => true);
			raise constraint_error;
		end if;
	end to_terminal_shape_smt;

	
	function to_package_model (
	-- Builds a package model from the given lines.
		package_name	: in et_libraries.type_component_package_name.bounded_string; -- S_SO14
		lines			: in et_pcb.type_lines.list;
		log_threshold	: in et_string_processing.type_log_level)
		return et_pcb.type_package is
		
		use et_pcb;
		use et_pcb.type_lines;
		line_cursor : et_pcb.type_lines.cursor := lines.first; -- points to the line being processed

		ob : constant character := '(';
		cb : constant character := ')';

		term_char_seq : constant string (1..2) := latin_1.space & ')';
		term_char_set : character_set := to_set (term_char_seq);

		-- the section prefix is a workaround due to GNAT reserved keywords.
		sec_prefix : constant string (1..4) := "sec_";
		type type_section is (
			INIT,
			SEC_AT,
			SEC_ATTR,
			SEC_ANGLE,
			SEC_CENTER,
			--SEC_CLEARANCE,
			SEC_DESCR,
			SEC_DRILL,
			SEC_EFFECTS,
			SEC_END,
			SEC_FONT,
			SEC_FP_ARC,
			SEC_FP_CIRCLE,
			SEC_FP_LINE,
			SEC_FP_TEXT,
			--SEC_JUSTIFY,
			SEC_LAYER,
			SEC_LAYERS,
			SEC_MODEL,
			SEC_MODULE,
			SEC_PAD,
			SEC_ROTATE,
			SEC_SCALE,
			SEC_SIZE,
			--SEC_SOLDER_MASK_MARGIN,
			SEC_START,
			SEC_TAGS,
			SEC_TEDIT,
			SEC_THICKNESS,
			SEC_WIDTH,
			SEC_XYZ
			);

		function to_string (section : in type_section) return string is
			len : positive := type_section'image (section)'last;
		begin
			return to_lower (type_section'image (section)(sec_prefix'last+1 ..len));
		end to_string;
	
		function expect_keyword (section : in type_section) return string is
			len : positive := to_string (section)'last;
		begin
			return "expect keyword '" & to_string (section) & "'";
		end expect_keyword;

		function enter_section (section : in type_section) return string is begin
			return ("entering section " & to_string (section));
		end enter_section;

		function return_to_section (section : in type_section) return string is begin
			return ("returning to section " & to_string (section));
		end return_to_section;

		function process_section (section : in type_section) return string is begin
			return ("processing section " & to_string (section));
		end process_section;
	
		entry_length_max : constant positive := 200;
		package type_argument is new generic_bounded_length (entry_length_max);

		type type_argument_counter is range 0..3;
		function to_string (arg_count : in type_argument_counter) return string is begin
			return trim (type_argument_counter'image (arg_count), left);
		end to_string;			

		timestamp : type_timestamp;
		description : type_package_description.bounded_string;
		tags : type_package_tags.bounded_string;

		type type_attribute is (SMD, THT, VIRTUAL);
		package_technology : type_assembly_technology := THT; -- according to the majority of terminals
		package_appearance : type_package_appearance := REAL;
		
		type type_text_base is new type_text with null record;
		text_basic : type_text_base;
		text : type_text_with_content;
		text_content : et_libraries.type_text_content.bounded_string;
		placeholder : type_package_text_placeholder;

		fp_text_meaning : type_fp_text_meaning;
		fp_text_hidden : boolean;
		
		line_start, line_end : et_pcb_coordinates.type_point_3d;
		line_width : et_pcb_coordinates.type_distance;
		
		terminal_name : et_libraries.type_terminal_name.bounded_string;
		terminal_technology : type_assembly_technology;
		terminal_shape_tht : type_terminal_shape_tht;
		terminal_shape_smt : type_terminal_shape_smt;
		terminal_inserted : boolean;

-- 		terminal_copper_width_outer_layers : et_pcb_coordinates.type_distance;
		terminal_copper_width_inner_layers : et_pcb_coordinates.type_distance := 1.0; -- CS load from DRU ?
		
		terminal_top_solder_paste, terminal_bot_solder_paste : type_terminal_solder_paste;
		terminal_solder_paste : type_terminal_solder_paste;
		
		terminal_top_stop_mask, terminal_bot_stop_mask : type_terminal_stop_mask;
		terminal_stop_mask : type_terminal_stop_mask;

		terminals : type_terminals.map;

		-- CS use subtypes for reasonable sizes below:
		object_size_x, object_size_y : et_pcb_coordinates.type_distance;
		drill_size : et_pcb_coordinates.type_distance; 
		object_position : et_pcb_coordinates.type_point_3d;
		object_angle : et_pcb_coordinates.type_angle;
		object_face : et_pcb_coordinates.type_face;

		type type_layer is (
			TOP_SILK, BOT_SILK,
			TOP_ASSY, BOT_ASSY,
			TOP_KEEP, BOT_KEEP
			);

		type type_line is new et_pcb.type_line with record
			width	: et_pcb_coordinates.type_distance;
			layer	: type_layer;
		end record;
		line : type_line;

		type type_arc is new et_pcb.type_arc with record
			width 	: et_pcb_coordinates.type_distance;
			angle 	: et_pcb_coordinates.type_angle;
			layer	: type_layer;
		end record;
		arc : type_arc;

		type type_circle is new et_pcb.type_circle with record
			width : et_pcb_coordinates.type_distance;
			point : et_pcb_coordinates.type_point_3d;
		end record;
		circle : type_circle;

		terminal_face : et_pcb_coordinates.type_face;
		terminal_position : et_pcb_coordinates.type_point_3d;
		terminal_size_x : et_pcb_coordinates.type_distance;
		terminal_size_y : et_pcb_coordinates.type_distance;		
		terminal_angle : et_pcb_coordinates.type_angle;
		

		object_layer : type_layer;
		text_layer : type_layer;

		type type_text is new et_pcb.type_text with record
			content	: et_libraries.type_text_content.bounded_string;
			layer	: type_layer;
		end record;
		text_2 : type_text; -- CS rename to text
		
		top_silk_screen, bot_silk_screen 	: type_package_silk_screen;
		top_assy_doc, bot_assy_doc			: type_package_assembly_documentation;
		top_keepout, bot_keepout			: type_package_keepout;

		pcb_contours			: type_package_pcb_contour;		
		pcb_contours_plated 	: type_package_pcb_contour_plated;
		route_restrict 			: type_package_route_restrict;
		via_restrict 			: type_package_via_restrict;
-- 		procedure init_object is 
-- 			use et_pcb_coordinates;
-- 		begin
-- 			object_size_x := zero_distance;
-- 			object_size_y := zero_distance;
-- 			object_drill_size := zero_distance;
-- 			--CS object_position := point_zero;
-- 			object_angle := zero_angle;
-- 			object_face := TOP;
-- 		end init_object;
		
		procedure init_terminal_layers is begin
			terminal_top_solder_paste := type_terminal_solder_paste'first;
			terminal_bot_solder_paste := type_terminal_solder_paste'first;
			terminal_top_stop_mask := type_terminal_stop_mask'first;
			terminal_bot_stop_mask := type_terminal_stop_mask'first;
		end init_terminal_layers;

		procedure set_stop_and_mask is 
			use et_pcb_coordinates;
			
			procedure invalid is begin
				log_indentation_reset;
				log (message_error & "contradicting layers in terminal !", console => true);
				log ("face " & to_string (object_face), console => true);
				log (" solder paste top " & to_string (terminal_top_solder_paste), console => true);
				log (" solder paste bot " & to_string (terminal_bot_solder_paste), console => true);
				log (" stop mask top    " & to_string (terminal_top_stop_mask), console => true);
				log (" stop mask bot    " & to_string (terminal_bot_stop_mask), console => true);
				raise constraint_error;
			end invalid; 
				
		begin -- set_stop_and_mask
			case object_face is
				when TOP =>
					terminal_solder_paste := terminal_top_solder_paste;
					if terminal_bot_solder_paste = APPLIED then
						invalid;
					end if;
					
					terminal_stop_mask := terminal_top_stop_mask;
					if terminal_bot_stop_mask = OPEN then
						invalid;
					end if;

				when BOTTOM =>
					terminal_solder_paste := terminal_bot_solder_paste;
					if terminal_top_solder_paste = APPLIED then
						invalid;
					end if;
					
					terminal_stop_mask := terminal_bot_stop_mask;
					if terminal_top_stop_mask = OPEN then
						invalid;
					end if;
			end case;
		end set_stop_and_mask;
		
		type type_section_and_argument_counter is record
			name 		: type_section := INIT;
-- 			parent		: type_section;
			arg_counter	: type_argument_counter := type_argument_counter'first;
		end record;

		section : type_section_and_argument_counter;
		parent_section : type_section;
		
		package sections_stack is new et_general.stack_lifo (max => 20, item => type_section_and_argument_counter);

		line_length_max : constant positive := 200;
		package type_current_line is new generic_bounded_length (line_length_max);
		use type_current_line;
		current_line : type_current_line.bounded_string;
		character_cursor : natural;

		procedure get_next_line is
		-- Fetches a new line. 
		begin
			next (line_cursor);
			if line_cursor /= et_pcb.type_lines.no_element then
				current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
				log ("line " & to_string (current_line), log_threshold + 4);
			else
				-- no more lines -- CS raise error ?
				null;
			end if;
		end get_next_line;
		
		procedure p1 is
		-- Updates the cursor position to the position of the next
		-- non_space character starting from the current cursor position.
		-- Fetches a new line if no further characters after current cursor position.
		begin
			character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
			while character_cursor = 0 loop
				get_next_line;
				character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
			end loop;
		end p1;

		procedure read_section is 
		-- Stores the section name and current argument counter on sections_stack.
		-- Reads the section name from current cursor position until termination
		-- character or its last character.
			end_of_kw : integer;  -- may become negative if no terminating character present
		begin
			-- save previous section and argument counter on stack
-- 			section.parent := section.name;
			sections_stack.push (section);
			parent_section := section.name;
			
			section.arg_counter := 0;
			
			-- get position of last character
			end_of_kw := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

			-- if no terminating character found, end_of_kw assumes length of line
			if end_of_kw = -1 then
				end_of_kw := length (current_line);
			end if;

			-- Compose section name from cursor..end_of_kw.
			-- This is an implicit general test whether the keyword is a valid keyword.
			section.name := type_section'value (sec_prefix & slice (current_line, character_cursor, end_of_kw));

			-- update cursor
			character_cursor := end_of_kw;

			log (enter_section (section.name), log_threshold + 3);

			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);

						log (message_error & "section '" & slice (current_line, character_cursor, end_of_kw) 
							& "' invalid or not supported yet", console => true);
						raise;
			
		end read_section;
		

		procedure read_arg is
		-- Reads the arguments of a section.
		-- Increments the argument counter after each argument.
		-- Validates the arguments according to the current section.
		-- Leaves the cursor at the position of the last character of the argument.
		-- If the argument was enclosed in quotations the cursor is left at
		-- the position of the trailing quotation.
			end_of_arg : integer; -- may become negative if no terminating character present

			use type_argument;
			use et_libraries;
			use et_libraries.type_text_content;
			use et_pcb_coordinates;
		
			arg : type_argument.bounded_string; -- here the argument goes temporarily

			procedure invalid_layer is begin
				log_indentation_reset;
				log (message_error & "invalid layer " & to_string (arg), console => true);
				raise constraint_error;
			end invalid_layer;

			procedure too_many_arguments is begin
				log_indentation_reset;
				log (message_error & "too many arguments in section " & to_string (section.name) & " !", console => true);
				log ("excessive argument reads '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end too_many_arguments;

			procedure invalid_fp_text_keyword is begin
				log_indentation_reset;
				log (message_error & "expect keyword '" & keyword_fp_text_reference 
					 & "' or '" & keyword_fp_text_value 
					 & "' or '" & keyword_fp_text_user
					 & "' ! found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_fp_text_keyword;

			procedure invalid_placeholder_reference is begin
				log_indentation_reset;
				log (message_error & "expect reference placeholder '" & placeholder_reference & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_placeholder_reference;

			procedure invalid_placeholder_value is
			begin
				log_indentation_reset;
				log (message_error & "expect value placeholder '" & to_string (package_name) & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_placeholder_value;

			procedure invalid_package_name is
			begin
				log_indentation_reset;
				log (message_error & "expect package name '" & to_string (package_name) & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_package_name;

			procedure invalid_component_assembly_face is
			begin
				log_indentation_reset;
				log (message_error & "default assembly face " & et_pcb_coordinates.to_string (BOTTOM) 
					 & " found. Must be " & et_pcb_coordinates.to_string (TOP) & " !", console => true);
				raise constraint_error;
			end invalid_component_assembly_face;

			procedure invalid_attribute is
			begin
				log_indentation_reset;
				log (message_error & "invalid attribute !", console => true);
				raise constraint_error;
			end invalid_attribute;

			procedure invalid_section is
			begin
				log_indentation_reset;
				log (message_error & "invalid subsection '" & to_string (section.name) 
					 & "' in parent section '" & to_string (parent_section) & "' !", console => true);
				raise constraint_error;
			end invalid_section;
				
		begin -- read_arg
			-- We handle an argument that is wrapped in quotation different from a non-wrapped argument:
			if element (current_line, character_cursor) = latin_1.quotation then
				-- Read the quotation-wrapped argument (strip quotations)

				-- get position of last character (before trailing quotation)
				end_of_arg := index (source => current_line, from => character_cursor + 1, pattern => 1 * latin_1.quotation) - 1;

				-- if no trailing quotation found -> error
				if end_of_arg = -1 then
					log_indentation_reset;
					log (message_error & affected_line (element (line_cursor))
						& latin_1.space & latin_1.quotation & " expected");
						raise constraint_error;
				end if;

				-- compose argument from first character after quotation until end_of_arg
				arg := to_bounded_string (slice (current_line, character_cursor + 1, end_of_arg));

				-- update cursor (to position of trailing quotation)
				character_cursor := end_of_arg + 1;
			else
				-- Read the argument from current cursor position until termination
				-- character or its last character.

				-- get position of last character
				end_of_arg := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

				-- if no terminating character found, end_of_arg assumes length of line
				if end_of_arg = -1 then
					end_of_arg := length (current_line);
				end if;

				-- compose argument from cursor..end_of_arg
				arg := to_bounded_string (slice (current_line, character_cursor, end_of_arg));

				-- update cursor
				character_cursor := end_of_arg;
			end if;

			section.arg_counter := section.arg_counter + 1;
			
			log ("arg" & to_string (section.arg_counter) & latin_1.space & to_string (arg), log_threshold + 4);

			-- Validate arguments according to current section.
			-- Load variables. When a section closes, the variables are used to build an object. see exec_section.
			case section.name is
				when SEC_MODULE =>
					case section.arg_counter is
						when 0 => null;
						when 1 =>
							if to_string (arg) /= to_string (package_name) then
								invalid_package_name;
							end if;
						when others => 
							too_many_arguments;
					end case;

				when SEC_DESCR =>
					case section.arg_counter is
						when 0 => null;
						when 1 =>
							-- CS check length
							description := type_package_description.to_bounded_string (to_string (arg));
							-- CS check description
						when others => 
							too_many_arguments;
					end case;

				when SEC_TAGS =>
					case section.arg_counter is
						when 0 => null;
						when 1 =>
							-- CS check length
							tags := type_package_tags.to_bounded_string (to_string (arg));
							-- CS check tags
						when others => 
							too_many_arguments;
					end case;
					
				when SEC_TEDIT =>
					case section.arg_counter is
						when 0 => null;
						when 1 =>
							-- CS check length
							timestamp := type_timestamp (to_string (arg));
							et_string_processing.check_timestamp (timestamp);
						when others => 
							too_many_arguments;
					end case;

				when SEC_ATTR =>
					case section.arg_counter is
						when 0 => null;
						when 1 =>
							-- CS check length
							if to_string (arg) = attribute_technology_smd then
								package_technology := SMT; -- overwrite default (see declarations)
							elsif to_string (arg) = attribute_technology_virtual then
								package_appearance := VIRTUAL;  -- overwrite default (see declarations)
							else
								invalid_attribute;
							end if;
						when others => 
							too_many_arguments;
					end case;
					
				when SEC_FP_TEXT =>
					case parent_section is
						when SEC_MODULE =>
							text_2.hidden := false; -- "hide" flag is optionally provided as last argument. if not, default to false
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = keyword_fp_text_reference then
										fp_text_meaning := REFERENCE;
									elsif to_string (arg) = keyword_fp_text_value then
										fp_text_meaning := VALUE;
									elsif to_string (arg) = keyword_fp_text_user then
										fp_text_meaning := USER;
									else
										invalid_fp_text_keyword;
									end if;
									
								when 2 => 
									case fp_text_meaning is
										when REFERENCE => 
											if to_string (arg) /= placeholder_reference then
												invalid_placeholder_reference;
											end if;

										when VALUE =>
											if to_string (arg) /= to_string (package_name) then
												invalid_placeholder_value;
											end if;

										when USER =>
											-- CS length check
											text_2.content := to_bounded_string (to_string (arg));
											-- CS character check
									end case;
									
								when 3 => 
									if to_string (arg) = keyword_fp_text_hide then
										text_2.hidden := true;
									end if;
									
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				when SEC_CENTER =>
					case parent_section is
						when SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => circle.center, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => circle.center, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => circle.center, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_START =>
					case parent_section is
						when SEC_FP_LINE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => line.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => line.start_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => line.start_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => arc.center, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => arc.center, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => arc.center, value => zero_distance);
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;

				when SEC_END =>
					case parent_section is
						when SEC_FP_LINE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => line.end_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => line.end_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => line.end_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => arc.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => arc.start_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => arc.start_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => circle.point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => circle.point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => circle.point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				when SEC_ANGLE =>
					case parent_section is
						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => arc.angle := to_angle (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_LAYER =>
					case parent_section is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_bot_copper then
										invalid_component_assembly_face;
									elsif to_string (arg) /= layer_top_copper then
										invalid_layer;
									end if;
								when others => too_many_arguments;
							end case;
									
						when SEC_FP_TEXT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										text_2.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										text_2.layer := BOT_SILK;
									elsif to_string (arg) = layer_top_assy_doc then
										text_2.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										text_2.layer := BOT_ASSY;
									elsif to_string (arg) = layer_top_keepout then
										text_2.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										text_2.layer := BOT_KEEP;
									else
										invalid_layer; -- CS copper layers ?
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_FP_LINE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										line.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										line.layer := BOT_SILK;
									elsif to_string (arg) = layer_top_assy_doc then
										line.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										line.layer := BOT_ASSY;
									elsif to_string (arg) = layer_top_keepout then
										line.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										line.layer := BOT_KEEP;
									else
										invalid_layer; -- CS copper layers ?
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC => -- SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										arc.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										arc.layer := BOT_SILK;
									elsif to_string (arg) = layer_top_assy_doc then
										arc.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										arc.layer := BOT_ASSY;
									elsif to_string (arg) = layer_top_keepout then
										arc.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										arc.layer := BOT_KEEP;
									else
										invalid_layer; -- CS copper layers ?
									end if;

								when others => too_many_arguments;
							end case;

							
						when others => invalid_section;
					end case;

				when SEC_WIDTH =>
					case parent_section is
						when SEC_FP_LINE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => line.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => arc.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => circle.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				when SEC_SIZE =>
					case parent_section is
						when SEC_FONT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => text_2.size_x := to_distance (to_string (arg));
								when 2 => text_2.size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PAD =>
							case section.arg_counter is
								when 0 => null;
								when 1 => terminal_size_x := to_distance (to_string (arg));
								when 2 => terminal_size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;
					
				when SEC_THICKNESS =>
					case parent_section is
						when SEC_FONT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => text_2.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_AT =>
					case parent_section is
						when SEC_PAD =>
							terminal_angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => terminal_position, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => terminal_position, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => terminal_position, value => zero_distance);
								when 3 => 
									terminal_angle := to_angle (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_FP_TEXT =>
							text.angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => text_2.position, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => text_2.position, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => text_2.position, value => zero_distance);
								when 3 => 
									text.angle := to_angle (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;
							
				when SEC_DRILL =>
					case parent_section is
						when SEC_PAD =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									drill_size := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_LAYERS => -- applies for terminals exclusively
					case parent_section is
						when SEC_PAD =>
							case section.arg_counter is
								when 0 => null;	
								when others => 	
									case terminal_technology is
										when SMT =>

											-- copper
											if to_string (arg) = layer_top_copper then
												terminal_face := TOP;
											elsif to_string (arg) = layer_bot_copper then
												terminal_face := BOTTOM;

											-- solder paste
											elsif to_string (arg) = layer_top_solder_paste then
												terminal_top_solder_paste := APPLIED;
											elsif to_string (arg) = layer_bot_solder_paste then
												terminal_bot_solder_paste := APPLIED;

											-- stop mask
											elsif to_string (arg) = layer_bot_stop_mask then
												terminal_bot_stop_mask := OPEN;
											elsif to_string (arg) = layer_top_stop_mask then
												terminal_top_stop_mask := OPEN;

											else
												invalid_layer;
											end if;

												
										when THT =>

											-- copper and stop mask
											if to_string (arg) = layer_all_copper 
											or to_string (arg) = layer_all_stop_mask then
												null; -- fine
											else
												invalid_layer;
											end if;
											
									end case;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_PAD =>
					case parent_section is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								
								when 1 => null;
									-- CS: check terminal name length
									terminal_name := to_terminal_name (to_string (arg));
									-- CS: check characters
								when 2 =>
									terminal_technology := to_assembly_technology (to_string (arg));
								when 3 =>
									case terminal_technology is
										when SMT => terminal_shape_smt := to_terminal_shape_smt (to_string (arg));
										when THT => terminal_shape_tht := to_terminal_shape_tht (to_string (arg));
									end case;
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
					
				when others => null;
			end case;
			
			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (ada.exceptions.exception_message (event));
						raise;

		end read_arg;

		procedure exec_section is
		-- Performs an operation according to the active section and variables that have been
		-- set earlier (when processing the arguments. see procedure read_arg).
		-- Restores the previous section name and argument counter.
			use et_pcb_coordinates;
			use et_libraries;
			terminal_cursor			: type_terminals.cursor;
			silk_screen_line_cursor	: type_silk_lines.cursor;

			type type_arc is new et_pcb.type_arc with null record;
			arc : type_arc;
		
			procedure invalid_layer_reference is begin
				log_indentation_reset;
				log (message_error & "reference placeholder must be in a silk screen layer !", console => true);
				raise constraint_error;
			end invalid_layer_reference;

			procedure invalid_layer_value is begin
				log (message_warning & "value placeholder should be in a fabrication layer !");
			end invalid_layer_value;

			procedure invalid_layer_user is begin
				log_indentation_reset;
				log (message_error & "user text must be in a silk screen or fabrication layer !", console => true);
				raise constraint_error;
			end invalid_layer_user;

		begin -- exec_section
			log (process_section (section.name), log_threshold + 4);
			case section.name is

				when SEC_TEDIT =>
					log ("timestamp " & string (timestamp), log_threshold + 1);

				when SEC_DESCR =>
					log (to_string (description), log_threshold + 1);
					
				when SEC_TAGS =>
					log (to_string (tags), log_threshold + 1);

				when SEC_ATTR =>
					null;
					
				when SEC_START =>
					line_start := object_position;

				when SEC_END =>
					line_end := object_position;

				when SEC_FONT =>
					text_basic.size_x		:= object_size_x;
					text_basic.size_y		:= object_size_y;
					text_basic.width 		:= line_width;
					text_basic.angle 		:= object_angle;
					text_basic.alignment 	:= (horizontal => CENTER, vertical => BOTTOM);
					
				when SEC_FP_TEXT =>
					text_basic.position		:= object_position;
					text_basic.hidden		:= fp_text_hidden;

					case fp_text_meaning is
						when REFERENCE =>
							placeholder := (et_pcb.type_text (text_2) with meaning => REFERENCE);
							
							case text_2.layer is
								when TOP_SILK =>
									top_silk_screen.placeholders.append (placeholder);
									placeholder_silk_screen_properties (TOP, top_silk_screen.placeholders.last, log_threshold + 1);
								when BOT_SILK =>
									bot_silk_screen.placeholders.append (placeholder);
									placeholder_silk_screen_properties (BOTTOM, bot_silk_screen.placeholders.last, log_threshold + 1);
								when others => -- should never happen
									invalid_layer_reference; 
							end case;

						when VALUE =>
							placeholder := (et_pcb.type_text (text_2) with meaning => VALUE);
							
							case text_2.layer is
								when TOP_ASSY =>
									top_assy_doc.placeholders.append (placeholder);
									placeholder_assy_doc_properties (TOP, top_assy_doc.placeholders.last, log_threshold + 1);
								when BOT_ASSY =>
									bot_assy_doc.placeholders.append (placeholder);
									placeholder_assy_doc_properties (BOTTOM, bot_assy_doc.placeholders.last, log_threshold + 1);
								when others => -- should never happen
									invalid_layer_value;
							end case;
							
						when USER =>
							--text := (et_pcb.type_text (text_basic) with content => text_content);
							case text_2.layer is
								when TOP_SILK => 
									top_silk_screen.texts.append ((et_pcb.type_text (text_2) with content => text_2.content));
									text_silk_screen_properties (TOP, top_silk_screen.texts.last, log_threshold + 1);
								when BOT_SILK => 
									bot_silk_screen.texts.append ((et_pcb.type_text (text_2) with content => text_2.content));
									text_silk_screen_properties (BOTTOM, bot_silk_screen.texts.last, log_threshold + 1);
								when TOP_ASSY => 
									top_assy_doc.texts.append ((et_pcb.type_text (text_2) with content => text_2.content));
									text_assy_doc_properties (TOP, top_assy_doc.texts.last, log_threshold + 1);
								when BOT_ASSY => 
									bot_assy_doc.texts.append ((et_pcb.type_text (text_2) with content => text_2.content));
									text_assy_doc_properties (BOTTOM, bot_assy_doc.texts.last, log_threshold + 1);
								when others -- should never happen 
									=> invalid_layer_user;
							end case;
					end case;
					
				when SEC_FP_LINE =>
					-- Append the line to the container correspoinding to the layer. Then log the line properties.
					case object_layer is
						when TOP_SILK =>
							top_silk_screen.lines.append ((line_start, line_end, line_width));
							line_silk_screen_properties (TOP, top_silk_screen.lines.last, log_threshold + 1);
						when BOT_SILK =>
							bot_silk_screen.lines.append ((line_start, line_end, line_width));
							line_silk_screen_properties (BOTTOM, bot_silk_screen.lines.last, log_threshold + 1);
						when TOP_ASSY =>
							top_assy_doc.lines.append ((line_start, line_end, line_width));
							line_assy_doc_properties (TOP, top_assy_doc.lines.last, log_threshold + 1);
						when BOT_ASSY =>
							bot_assy_doc.lines.append ((line_start, line_end, line_width));
							line_assy_doc_properties (BOTTOM, bot_assy_doc.lines.last, log_threshold + 1);
						when TOP_KEEP =>
							top_keepout.lines.append ((line_start, line_end));
							line_keepout_properties (TOP, top_keepout.lines.last, log_threshold + 1);
						when BOT_KEEP =>
							bot_keepout.lines.append ((line_start, line_end));
							line_keepout_properties (BOTTOM, top_keepout.lines.last, log_threshold + 1);
					end case;

				when SEC_FP_ARC =>
					-- Append the arc to the container correspoinding to the layer. Then log the arc properties.
					arc := (center => line_start, start_point => line_end,
							end_point => line_end); -- CS must be calculated
					
					case object_layer is
						when TOP_SILK =>
							top_silk_screen.arcs.append ((et_pcb.type_arc (arc) with line_width));
							arc_silk_screen_properties (TOP, top_silk_screen.arcs.last, log_threshold + 1);
							
						when BOT_SILK =>
							bot_silk_screen.arcs.append ((et_pcb.type_arc (arc) with line_width));
							arc_silk_screen_properties (BOTTOM, bot_silk_screen.arcs.last, log_threshold + 1);
							
						when TOP_ASSY =>
							top_assy_doc.arcs.append ((et_pcb.type_arc (arc) with line_width));
							arc_assy_doc_properties (TOP, top_assy_doc.arcs.last, log_threshold + 1);
							
						when BOT_ASSY =>
							bot_assy_doc.arcs.append ((et_pcb.type_arc (arc) with line_width));
							arc_assy_doc_properties (BOTTOM, bot_assy_doc.arcs.last, log_threshold + 1);
							
						when TOP_KEEP =>
							top_keepout.arcs.append ((
								center 		=> arc.center,
								start_point	=> arc.start_point, 
								end_point	=> arc.end_point));
							arc_keepout_properties (TOP, top_keepout.arcs.last, log_threshold + 1);
							
						when BOT_KEEP =>
							bot_keepout.arcs.append ((
								center 		=> arc.center,
								start_point	=> arc.start_point, 
								end_point	=> arc.end_point));
							arc_keepout_properties (BOTTOM, top_keepout.arcs.last, log_threshold + 1);
					end case;
					
				when SEC_PAD =>

					-- Insert a terminal in the list "terminals":
					case terminal_technology is
						when THT =>

							if terminal_shape_tht = CIRCULAR then
								terminals.insert (
									key 		=> terminal_name,
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
													technology 		=> THT,
													shape 			=> CIRCULAR,
													tht_hole		=> DRILLED,
													width_inner_layers => terminal_copper_width_inner_layers,
													drill_size_cir	=> drill_size,
													shape_tht		=> terminal_shape_tht,
													position		=> type_terminal_position (to_terminal_position (object_position, object_angle))
												   ));
							else
								terminals.insert (
									key 		=> terminal_name,
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
													technology 		=> THT,
													shape			=> NON_CIRCULAR,
													tht_hole		=> DRILLED,
													width_inner_layers => terminal_copper_width_inner_layers,
													drill_size_dri	=> drill_size,
													shape_tht		=> terminal_shape_tht,
													position		=> type_terminal_position (to_terminal_position (object_position, object_angle)),
													size_tht_x		=> object_size_x,
													size_tht_y		=> object_size_y
												));
							end if;

							
						when SMT =>

							set_stop_and_mask;
							
							if terminal_shape_smt = CIRCULAR then
								terminals.insert (
									key 		=> terminal_name, 
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
													technology 		=> SMT,
													shape			=> CIRCULAR,
													tht_hole		=> DRILLED, -- has no meaning here
													shape_smt		=> terminal_shape_smt,
													position		=> type_terminal_position (to_terminal_position (object_position, object_angle)),
													face 			=> object_face,
													stop_mask		=> terminal_stop_mask,
													solder_paste	=> terminal_solder_paste
												));
							else
								terminals.insert (
									key 		=> terminal_name, 
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
													technology 		=> SMT,
													shape			=> NON_CIRCULAR,
													tht_hole		=> DRILLED, -- has no meaning here
													shape_smt		=> terminal_shape_smt,
													position		=> type_terminal_position (to_terminal_position (object_position, object_angle)),
													face 			=> object_face,
													stop_mask		=> terminal_stop_mask,
													solder_paste	=> terminal_solder_paste,
													size_smt_x		=> object_size_x,
													size_smt_y		=> object_size_y
												));
							end if;

							init_terminal_layers;
					end case;


					if terminal_inserted then
						terminal_properties (terminal_cursor, log_threshold + 1);
					else
						log_indentation_reset;
						log (message_error & "duplicated terminal " & to_string (terminal_name) & " !");
						raise constraint_error;
					end if;
					
				when others => null;
			end case;

			-- restore previous section and argument counter from stack
			section := sections_stack.pop;
			log (return_to_section (section.name), log_threshold + 3);
			
			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (ada.exceptions.exception_message (event));
						raise;
			
		end exec_section;
		
		procedure check_placeholders is
		-- Checks if there is at least one placeholder for reference and for value.
		-- CS: validate text sizes and width according to specifications in configuration file
			use et_pcb_coordinates;
			use type_package_text_placeholders;
			cursor : type_package_text_placeholders.cursor;
			placeholder : type_package_text_placeholder;
			reference_found, value_found : boolean := false;
		begin
			-- There must be a placeholder for the reference in the top silk screen:
			cursor := top_silk_screen.placeholders.first;
			while cursor /= type_package_text_placeholders.no_element loop
				placeholder := element (cursor);
				if placeholder.meaning = REFERENCE then
					reference_found := true;
					exit;
				end if;
				next (cursor);
			end loop;

			if not reference_found then
				log_indentation_reset;
				log (message_error & "no placeholder for component " 
					 & to_string (REFERENCE) 
					 & " found in " & to_string (TOP) & " silk screen !", console => true);
				raise constraint_error;
			end if;

			-- There must be a placeholder for the value in the top assembly documentation:
			cursor := top_assy_doc.placeholders.first;
			while cursor /= type_package_text_placeholders.no_element loop
				placeholder := element (cursor);
				if placeholder.meaning = VALUE then
					value_found := true;
					exit;
				end if;
				next (cursor);
			end loop;

			if not value_found then
				log_indentation_reset;
				log (message_error & "no placeholder for component " 
					 & to_string (VALUE) 
					 & " found in " & to_string (TOP) & " assembly documentation !", console => true);
				raise constraint_error;
			end if;
			
		end check_placeholders;

		procedure check_technology is
		-- If the package is REAL, counts the tht and smd terminals. 
		-- Warns operator if the package technology
		-- is not set according to the majority of terminals respectively.
			use type_terminals;
			cursor : type_terminals.cursor := terminals.first;
			tht_count, smt_count : natural := 0; -- the number of THT or SMT terminals

			function number (count : in natural) return string is begin
				return " (" & trim (positive'image (count), left) & "). ";
			end number;
		
		begin -- check_technology
			log ("checking package technology vs. terminal count ...", log_threshold + 1);
			log_indentation_up;
			
			log ("appearance " & to_string (package_appearance), log_threshold + 1);
			
			if package_appearance = REAL then
				log ("assembly technology " & to_string (package_technology), log_threshold + 1);
			
				while cursor /= type_terminals.no_element loop
					case element (cursor).technology is
						when THT => tht_count := tht_count + 1;
						when SMT => smt_count := smt_count + 1;
					end case;
					next (cursor);
				end loop;

				case package_technology is
					when THT =>
						if tht_count < smt_count then
							log (message_warning & "majority of terminals is " & to_string (SMT)
								& number (smt_count)
								& "Package technology should be " & to_string (SMT) & " !");
						end if;

					when SMT =>
						if smt_count < tht_count then
							log (message_warning & "majority of terminals is " & to_string (THT)
								& number (tht_count)
								& "Package technology should be " & to_string (THT) & " !");
						end if;
				end case;

			end if;
			log_indentation_down;
		end check_technology;
		
	begin -- to_package_model
		log ("parsing/building model ...", log_threshold);
		log_indentation_up;

		sections_stack.init;

		-- get first line
		current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
		log ("line " & to_string (current_line), log_threshold + 4);
		
		-- get position of first opening bracket
		character_cursor := type_current_line.index (current_line, 1 * ob);

		init_terminal_layers;
-- 		init_object;
		
		loop
			<<label_1>>
				p1;
				read_section;
				p1;
				if element (current_line, character_cursor) = ob then goto label_1; end if;

			<<label_3>>
				read_arg;
				p1;
				-- Test for cb, ob or other character:
				case element (current_line, character_cursor) is

					-- If closing bracket after argument.
					when cb => goto label_2;

					-- If another section at a deeper level follows.
					when ob => goto label_1;

					-- In case another argument follows:
					when others => goto label_3; 
				end case;

			<<label_2>>
				exec_section;

				if sections_stack.depth = 0 then exit; end if;
				p1;

				-- Test for cb, ob or other character:
				case element (current_line, character_cursor) is

					-- If closing bracket after argument.
					when cb => goto label_2;

					-- If another section at a deeper level follows.
					when ob => goto label_1;

					-- In case an argument follows:
					when others => goto label_3; 
				end case;
				
		end loop;

		-- check section name. must be top level section
		if section.name /= INIT then
			log_indentation_reset;
			log (message_error & "top level section not closed !");
			raise constraint_error;
		end if;

		-- check the most relevant placeholders
		check_placeholders;

		-- check assembly technology vs. terminal count
		check_technology;

		-- CS validate description
		
		log_indentation_down;

		-- depending on the attribute we return a real or a virtual package
		case package_appearance is
			when REAL =>
				return (
					appearance				=> REAL,
					package_contours		=> no_contour, -- CS to be filled from 3d model
					pcb_contours			=> pcb_contours,
					pcb_contours_plated 	=> pcb_contours_plated,
					terminals				=> terminals,
					silk_screen				=> (top => top_silk_screen, bottom => bot_silk_screen),
					keepout					=> (top => top_keepout, bottom => bot_keepout),
					route_restrict 			=> route_restrict,
					via_restrict 			=> via_restrict,
					assembly_documentation 	=> (top => top_assy_doc, bottom => bot_assy_doc),
					timestamp				=> timestamp,
					description				=> description,
					technology				=> package_technology
					);

			when VIRTUAL => -- no package_contours
				return (
					appearance				=> VIRTUAL,
					pcb_contours			=> pcb_contours,
					pcb_contours_plated 	=> pcb_contours_plated,
					terminals				=> terminals,
					silk_screen				=> (top => top_silk_screen, bottom => bot_silk_screen),
					keepout					=> (top => top_keepout, bottom => bot_keepout),
					route_restrict 			=> route_restrict,
					via_restrict 			=> via_restrict,
					assembly_documentation 	=> (top => top_assy_doc, bottom => bot_assy_doc),
					timestamp				=> timestamp,
					description				=> description,
					technology				=> package_technology
					);
		end case;
				
	end to_package_model;
	
	procedure read_libraries (
	-- Reads package libraries. Root directory is et_libraries.lib_dir.
	-- The libraries in the container are named after the libraries found in lib_dir.
		log_threshold 	: in et_string_processing.type_log_level) is

		use ada.directories;
		use et_libraries;
		use et_general;
		use et_general.type_directory_entries;
		use et_pcb;

		-- backup the directory of origin
		use type_directory_name;
		origin_directory : type_directory_name.bounded_string := to_bounded_string (current_directory);
	
		-- After fetching the names of the package libraries, their names
		-- are stored here. When processing the list we use the library_name_cursor.
		library_names : type_directory_entries.list;
		library_name_cursor : type_directory_entries.cursor;

		-- While inserting the libraries the flag library_inserted goes true once
		-- inserting was successuful. It goes false if the library is already in the list.
		-- The library_cursor points to the library in the container package_libraries.
		library_inserted : boolean;
		library_cursor : et_pcb.type_libraries.cursor;

		procedure read_package_names (
		-- Creates empty packages in the package_libraries. The package names are
		-- named after the packages found in the library directories.
			library_name	: in type_full_library_name.bounded_string;
			packages		: in out type_packages.map) is

			package_names : type_directory_entries.list;
			package_name_cursor : type_directory_entries.cursor;
			
			library_handle : ada.text_io.file_type;
			line : type_fields_of_line; -- a line of a package model

			use et_pcb.type_lines;
			lines : et_pcb.type_lines.list; -- all lines of a single package model

		begin -- read_package_names
			log ("reading package names in " & current_directory & " ...", log_threshold + 3);
			log_indentation_up;

			package_names := directory_entries (
								target_directory	=> current_directory, 
								category			=> ada.directories.ordinary_file,
								pattern				=> et_kicad.package_pattern);

			-- show number of package libraries
			if is_empty (package_names) then
				log (message_warning & "library is empty !");
			else
				log ("found" & count_type'image (length (package_names)) & " packages", log_threshold + 4);
			end if;
			
			log_indentation_up;

			package_name_cursor := package_names.first;
			while package_name_cursor /= type_directory_entries.no_element loop
				log (element (package_name_cursor), log_threshold + 5);
				log_indentation_up;
				
				-- open package model file
				open (
					file => library_handle,
					mode => in_file,
					name => element (package_name_cursor)); -- S_0201.kicad_mod

				-- read lines of model file
				set_input (library_handle);
				while not end_of_file loop
-- 					log (get_line);

					-- Store line in variable "line" (see et_string_processing.ads)
					line := et_string_processing.read_line (
								line 			=> get_line,
								number 			=> ada.text_io.line (current_input),
								--delimiter_wrap	=> true, -- some things are enclosed in quotations
								ifs 			=> latin_1.space); -- fields are separated by space

					-- insert line in container "lines"
					if field_count (line) > 0 then -- we skip empty or commented lines
						append (lines, line);
					end if;
						
				end loop;
				close (library_handle);

				-- From the collected lines the package model can be built and inserted in the 
				-- package list right away:
				type_packages.insert (
					container	=> packages,
					key			=> to_package_name (base_name (element (package_name_cursor))), -- S_0201
					new_item	=> to_package_model (
										package_name 	=> to_package_name (base_name (element (package_name_cursor))), -- S_SO14
										lines			=> lines,
										log_threshold	=> log_threshold + 6));
				
				-- Once the package model file has been read, the collection of lines
				--must be cleared for the next model.
				clear (lines);

				log_indentation_down;
				next (package_name_cursor);
			end loop;

			log_indentation_down;
			log_indentation_down;

			exception
				when event:
					others =>
						log_indentation_reset;
						log (ada.exceptions.exception_message (event), console => true);
						raise;

		end read_package_names;

	
	begin -- read_libraries
		log ("reading package libraries ...", log_threshold);

		-- fetch package library names from lib_dir
		library_names := directory_entries (
							target_directory	=> et_libraries.to_string (et_libraries.lib_dir), 
							category			=> ada.directories.directory,
							pattern				=> et_kicad.package_library_pattern);

		log_indentation_up;

		-- Abort if there are no package libraries. Otherwise loop through the library names
		-- and create the libraries in container package_libraries.
		if is_empty (library_names) then
			log_indentation_reset;
			log (message_error & "no package libraries found !");
			raise constraint_error;
		else
			-- show number of package libraries
			log ("found" & count_type'image (length (library_names)) & " libraries", log_threshold + 1);
			log_indentation_up;

			-- Loop through library names and create the actual libraries in container package_libraries:
			library_name_cursor := library_names.first;
			while library_name_cursor /= type_directory_entries.no_element loop
				log ("reading " & element (library_name_cursor) & " ...", log_threshold + 2);

				-- create the (empty) library
				et_pcb.type_libraries.insert (
					container	=> package_libraries,
-- 					key			=> to_library_name (element (library_name_cursor)),
					key			=> to_full_library_name (
										root_dir => lib_dir,
										lib_name => to_library_name (element (library_name_cursor))),
					inserted	=> library_inserted,
					position	=> library_cursor,
					new_item	=> type_packages.empty_map);

				if library_inserted then
					log_indentation_up;
					
					-- change in library (the kicad package library is just a directory like ../lbr/bel_ic.pretty)
					set_directory (compose (to_string (lib_dir), element (library_name_cursor)));
					
					et_pcb.type_libraries.update_element (
						container	=> package_libraries,
						position	=> library_cursor,
						process		=> read_package_names'access);

					-- change back to directory of origin
					set_directory (et_pcb.to_string (origin_directory));
					log_indentation_down;
				else
					log_indentation_up;
					log ("already loaded -> skipped", log_threshold + 2);
					log_indentation_down;
				end if;
				
				next (library_name_cursor);
			end loop;

			log_indentation_down;
		end if;
		
		log_indentation_down;

		exception
			when event:
				others =>
					log_indentation_reset;
					put_line (ada.exceptions.exception_message (event));
					raise;

	end read_libraries;

	

	
end et_kicad_pcb;

-- Soli Deo Gloria

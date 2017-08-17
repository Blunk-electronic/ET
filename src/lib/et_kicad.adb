------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD                                --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_libraries;				--use et_libraries;
with et_schematic;				use et_schematic;

with et_geometry;				use et_geometry;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;

package body et_kicad is


	procedure invalid_field (line : in type_fields_of_line) is
	-- CS: display field meaning ?
		-- 	meaning : in et_general.type_text_meaning) return string is
	begin
		write_message(
			file_handle => current_output,
			text => message_error & et_string_processing.affected_line(line) & "invalid field !",
			console => true);
	
			-- CS: refine output.
		raise constraint_error;
	end invalid_field;

	
	function to_text_meaning (
	-- Extracts from a scheamtic field like "F 0 "#PWR01" H 2000 3050 50  0001 C CNN" its meaning.
	-- Extracts from a component field like "F0 "IC" 0 50 50 H V C CNN" its meaning.
	-- Since the fields start different in libaray and schematic we also need a flag that tells
	-- the function whether we are dealing with scheamtic or library fields.
		line : in type_fields_of_line;
		schematic : in boolean) -- set false if it is about fields in a library, true if it is about a schematic field	
		return et_libraries.type_text_meaning is

		function field (
			line		: in type_fields_of_line;
			position	: in positive) return string renames get_field_from_line;
		
		meaning : et_libraries.type_text_meaning;

		function strip_f ( text : in string) return string is
		-- removes the heading character from the given string.
		begin return text(text'first+1..text'last); end strip_f;

		function strip_id ( text : in string) return string is
		-- removes the trailing id from the given string.
		begin return text(text'first..text'first); end strip_id;
	
	begin -- to_text_meaning
		case schematic is
			when true =>

				-- In a schematic the meaning of a text field is identified by "F 0 ...".

				-- So the first thing to do is test if the letter F at the begin of the line:
				if field(line,1) = component_field_identifier then

					-- Then we test the field id.
					-- The field id must be mapped to the actual field meaning:
					case type_component_field_id'value(field(line,2)) is -- "0..2"
						when component_field_reference		=> meaning := et_libraries.reference;
						when component_field_value			=> meaning := et_libraries.value;
						when component_field_footprint		=> meaning := et_libraries.packge;
						when component_field_datasheet		=> meaning := et_libraries.datasheet;
						when component_field_function		=> meaning := et_libraries.p_function;
						when component_field_partcode		=> meaning := et_libraries.partcode;
						when component_field_commissioned	=> meaning := et_libraries.commissioned;
						when component_field_updated		=> meaning := et_libraries.updated;
						when component_field_author			=> meaning := et_libraries.author;
						when others => invalid_field (line);
					end case;

				else
					invalid_field (line);
				end if;
				
			when false =>

				-- In a library the meaning of a text field is identified by "F0 ...".
				
				-- So the first thing to do is test if the letter F at the begin of the line:
				if strip_id(field(line,1)) = component_field_identifier then
				
					case type_component_field_id'value ( strip_f (field(line,1) ) ) is
						when component_field_reference		=> meaning := et_libraries.reference;
						when component_field_value			=> meaning := et_libraries.value;
						when component_field_footprint		=> meaning := et_libraries.packge;
						when component_field_datasheet		=> meaning := et_libraries.datasheet;
						when component_field_function		=> meaning := et_libraries.p_function;
						when component_field_partcode		=> meaning := et_libraries.partcode;
						when component_field_commissioned	=> meaning := et_libraries.commissioned;
						when component_field_updated		=> meaning := et_libraries.updated;
						when component_field_author			=> meaning := et_libraries.author;
						when others => invalid_field (line);
					end case;

				else
					invalid_field (line);
				end if;

		end case;

		return meaning;

		exception
			when constraint_error =>
				invalid_field (line); -- CS: May display the affected line a second time in some cases.
				raise;
		
	end to_text_meaning;
								 
	function to_text_orientation ( text : in string) return et_general.type_orientation is
	-- Converts a kicad field text orientation character (H/V) to type_orientation.
	begin	
		case type_field_orientation'value(text) is
			when H => return deg_0;
			when V => return deg_90;
		end case;
	end to_text_orientation;
	
	function to_alignment_horizontal ( text : in string) return et_libraries.type_text_alignment_horizontal is
	-- Converts a horizontal kicad text alignment to type_text_alignment_horizontal.
		a : et_libraries.type_text_alignment_horizontal;
	begin
		case type_field_alignment_horizontal'value(text) is
			when L => a := et_libraries.left;
			when C => a := et_libraries.center;
			when R => a := et_libraries.right;
		end case;
		return a;
	end to_alignment_horizontal;

	function to_alignment_vertical ( text : in string) return et_libraries.type_text_alignment_vertical is
	-- Converts a vertical kicad text alignment to type_text_alignment_vertical.
	-- The given text is something like CNN. We are interested in the first character only.
		a : et_libraries.type_text_alignment_vertical;
		s : string (1..1) := text(text'first..text'first);
	begin
		case type_field_alignment_vertical'value(s) is
			when T => a := et_libraries.top;
			when C => a := et_libraries.center;
			when B => a := et_libraries.bottom;
		end case;
		return a;
	end to_alignment_vertical;

	function to_text_style (
	-- Converts a vertical kicad text style to type_text_style.
	-- The given style_in is something like CNN. We are interested in the 2nd and 3rd character only.
		style_in : in string;
		text : in boolean -- true if it is about the style of a text, false if it is about the style of a field
		-- Explanation: The style of a text is something like "~" or "Italic".
		-- The style of a field comes with the letters 2 and 3 of a string like CNN.
		) return et_libraries.type_text_style is
		a : et_libraries.type_text_style;
		s_field : string (1..2);
	
		procedure invalid_style is
		begin
			put_line(message_error & "invalid text style '" & style_in & "' !");
			raise constraint_error;
		end invalid_style;
		
	begin -- to_text_style
		--put_line("style_in: " & style_in);
		case text is
			when true =>
				if style_in = text_schematic_style_normal then
					a := et_libraries.type_text_style'first;
				elsif style_in = text_schematic_style_italic then
					a := et_libraries.italic;
				else
					invalid_style;
				end if;
				
			when false =>
				s_field := style_in(style_in'first+1..style_in'last);
				
				if    s_field = field_style_default then 		a := et_libraries.type_text_style'first;
				elsif s_field = field_style_bold then 			a := et_libraries.bold;
				elsif s_field = field_style_italic then 		a := et_libraries.italic;
				elsif s_field = field_style_italic_bold then 	a := et_libraries.italic_bold;
				else
					invalid_style;
				end if;
		end case;

		--put_line("    style " & to_lower(et_general.type_text_style'image(a)));
		
		return a;
	end to_text_style;
	
	function to_field_visible ( 
	-- Converts the kicad field visible flag to the type_text_visible.
	-- The parameter "schematic" tells whether to convert a schematic or a component library field.
		vis_in 		: in string; -- the string to be converted
		schematic	: in boolean -- set false if it is about fields in a library, true if it is about a schematic field
		-- Explanation: The visibility of fields in schematic is defined by something like "0001" or "0000".
		-- In component libraries it is defined by characters like V or I.
		)
		return et_libraries.type_text_visible is
		v_in_lib : type_library_field_visible;
		v_in_sch : type_schematic_field_visible;
		v_out : et_libraries.type_text_visible;
	begin
		case schematic is
			
			when true =>
				-- As the type_schematic_field_visible has letter V as workaround, we must 
				-- prepend it here to vis_in before converting to a type_schematic_field_visible:
				v_in_sch := type_schematic_field_visible'value(schematic_field_visibility_prefix & vis_in);
				case v_in_sch is
					when V0000 => v_out := et_libraries.yes; -- visible
					when V0001 => v_out := et_libraries.no;  -- invisible
				end case;

			when false =>
				v_in_lib := type_library_field_visible'value(vis_in);
				case v_in_lib is
					when V => v_out := et_libraries.yes;
					when I => v_out := et_libraries.no;
				end case;

		end case;

		--put_line(et_general.type_text_visible'image(v_out));		
		return v_out;
	end to_field_visible;

	function to_appearance ( line : in type_fields_of_line; schematic : in boolean) 
	-- Converts the apperance flag to type_component_appearance.
	-- The parameter "schematic" specifies whether we are dealing with a schematic
	-- or a library component.
	-- The appearance (power symbol or normal) is defined in the component library by P/N
	-- example: DEF 74LS00 IC 0 30 Y Y 4 F N
	-- In a schematic it is defined by a hash sign:
	-- example: L P3V3 #PWR07
		return et_general.type_component_appearance is
		comp_app	: et_general.type_component_appearance;
		lca			: type_library_component_appearance;

		function field (
			line		: in type_fields_of_line;
			position	: in positive) return string renames get_field_from_line;

		procedure invalid_appearance is
		-- CS: display field meaning ?
			-- 	meaning : in et_general.type_text_meaning) return string is
		begin
			write_message(
				file_handle => current_output,
				text => message_error & et_string_processing.affected_line(line) & "invalid visibility flag !",
				console => true);
		
				-- CS: refine output.
			raise constraint_error;
		end invalid_appearance;		

		--keyword_appears : constant string (1..8) := "appears ";
	begin -- to_appearance
		case schematic is

			when true =>
				--put(3 * latin_1.space & keyword_appears);
				
				-- If it is about a schematic component we just test if the first
				-- character of the 3ed subfield is a hash sign.
				if field(line,3)(field(line,3)'first) = schematic_component_power_symbol_prefix then
					comp_app := et_general.sch;
				else
					comp_app := et_general.sch_pcb;
				end if;
				
			when false =>
				--put(4 * latin_1.space & keyword_appears);
				
				-- If it is about a library component we test the whole letter
				-- in subfield #10.
				lca := type_library_component_appearance'value(field(line,10));

				-- Evaluate lca and set comp_app accordingly.
				case lca is
					when N =>
						comp_app := et_general.sch_pcb;
					when P => 
						comp_app := et_general.sch;
				end case;
		end case;
		
		return comp_app;

		exception 
			when constraint_error =>
				invalid_appearance;
				raise;
				
	end to_appearance;

	function to_alternative_representation ( line : in type_fields_of_line; schematic : in boolean)
	-- Converts the kicad alternative (deMorgan) representation to the et alternative representation.
	-- In a schematic it is expressed in a line like "U 2 1 5992967A". The 3rd field is the deMorgan flag.
		return et_schematic.type_alternative_representation is

		function field ( line : in type_fields_of_line; pos : in positive) return string 
			renames et_string_processing.get_field_from_line;
		
		rep_in : type_alternative_representation;
		rep_out : et_schematic.type_alternative_representation;
	begin
		rep_in := type_alternative_representation'value (field (line,3));

		case rep_in is
			when alternative_representation_yes =>
				rep_out := yes;

				-- CS: currently we do not support alternative representations
				write_message(
					file_handle => current_output,
					text => message_error & "alternative representation (DeMorgan) not supported currently !",
					console => true);
				
			when alternative_representation_no =>
				rep_out := no;
		end case;
		
		return rep_out;
		
		-- CS exception handler
	end to_alternative_representation;

	function field_content ( text_field : in et_libraries.type_text ) return string is
	-- Returns the content of the given text field as string.
	begin
		return et_libraries.type_text_content.to_string(text_field.content);
	end field_content;
	
	procedure read_components_libraries is
	-- Reads components from libraries as stored in lib_dir and project_libraries:
		use et_libraries.type_list_of_library_names;
		use et_libraries.type_library_full_name;
		cursor			: et_libraries.type_list_of_library_names.cursor := first(et_libraries.project_libraries);
		lib_file_name	: et_libraries.type_library_full_name.bounded_string;

		lib_cursor		: et_libraries.type_libraries.cursor;
		lib_inserted	: boolean;

		comp_cursor		: et_libraries.type_components.cursor;
		--comp_inserted	: boolean;
		
		
		procedure read_library is
			line			: type_fields_of_line;
			component_entered : boolean := false; -- goes true while a component section is being processed
			type type_active_section is ( none, fields, footprints, draw);
			active_section : type_active_section := none; -- indicates the subsection being processed

			component_name		: et_libraries.type_component_name.bounded_string; -- 74LS00
			prefix				: et_general.type_component_prefix.bounded_string; -- IC

			-- CS: variable for unknown field #4

			port_name_offset	: et_libraries.type_grid;
			pin_name_visible 	: et_libraries.type_pin_visible;
			port_name_visible	: et_libraries.type_port_visible;
			units_total			: type_unit_id;
			unit_swap_level		: et_libraries.type_unit_swap_level := et_libraries.unit_swap_level_default;
			appearance			: et_general.type_component_appearance;

			reference			: et_libraries.type_text(meaning => et_libraries.reference);
			value				: et_libraries.type_text(meaning => et_libraries.value);
			commissioned		: et_libraries.type_text(meaning => et_libraries.commissioned);
			updated				: et_libraries.type_text(meaning => et_libraries.updated);
			author				: et_libraries.type_text(meaning => et_libraries.author);
			footprint			: et_libraries.type_text(meaning => et_libraries.packge);
			datasheet			: et_libraries.type_text(meaning => et_libraries.datasheet);
			fnction				: et_libraries.type_text(meaning => et_libraries.p_function);
			partcode			: et_libraries.type_text(meaning => et_libraries.partcode);

			--unit_id				: type_unit_id;

			function to_swap_level ( swap_in : in string)
			-- Converts the kicad interchangeable flag to the et swap level.
			-- Since Kicad has only one swap level we convert to the lowest swap level available.
			-- Used when reading component libraries.	
				return et_libraries.type_unit_swap_level is
				i : type_symbol_interchangeable;
			begin
				put("    units interchangeable ");
				i := type_symbol_interchangeable'value(swap_in);
				case i is
					when L =>
						put_line("no");
						return 0; -- no swapping allowed
					when F =>
						put_line("yes");
						return 1; -- swapping allowed at this level
				end case;
			end to_swap_level;

			function to_pin_visibile ( vis_in : in string)
			-- Converts the kicad "show pin number" flag to the et type_pin_visible.
			-- Used when reading component libraries.		
				return et_libraries.type_pin_visible is
				v : type_show_pin_number;
			begin
				put("    pin/pad numbers ");
				v := type_show_pin_number'value(vis_in);
				case v is 
					when Y => 
						put_line("visible");
						return et_libraries.on;
					when N => 
						put_line("invisible");
						return et_libraries.off;
				end case;
			end to_pin_visibile;

			function to_port_visibile ( vis_in : in string)
			-- Converts the kicad "show pin name" flag to the et type_port_visible.
			-- Used when reading component libraries.		
				return et_libraries.type_port_visible is
				v : type_show_pin_name;
			begin
				put("    port names ");	
				v := type_show_pin_name'value(vis_in);
				case v is 
					when Y => 
						put_line("visible");
						return et_libraries.on;
					when N => 
						put_line("invisible");
						return et_libraries.off;
				end case;
			end to_port_visibile;

			function read_field (meaning : in et_libraries.type_text_meaning) return et_libraries.type_text is
			-- Reads general text field properties from subfields 3..9 and returns a type_text with 
			-- the meaning as given in parameter "meaning".
				text : et_libraries.type_text(meaning);
			begin
				-- field #:
				-- 3/4 : x/y coordinates
				-- 5 : size
				-- 6 : orientation (H/V)
				-- 7 : visible/invisible (V/I)
				-- 8 : aligment horizontal (R,C,L)
				-- 9 : aligment vertical (TNN, CNN, BNN) / font normal, italic, bold, bold_italic (TBI, TBN)

				text.content := et_libraries.type_text_content.to_bounded_string(strip_quotes(get_field_from_line(line,2)));
				text.position.x := et_libraries.type_grid'value(get_field_from_line(line,3));
				text.position.y := et_libraries.type_grid'value(get_field_from_line(line,4));
				text.size := et_libraries.type_text_size'value(get_field_from_line(line,5));
				text.orientation := to_text_orientation (get_field_from_line(line,6));
				
				text.visible := to_field_visible (
					vis_in		=> get_field_from_line(line,7),
					schematic	=> false);

				text.alignment.horizontal := to_alignment_horizontal(get_field_from_line(line,8));
				text.alignment.vertical   := to_alignment_vertical  (get_field_from_line(line,9));
				text.style := to_text_style (style_in => get_field_from_line(line,9), text => false);

				-- NOTE: text.line_width assumes default (see et_general.ads) as no line width is provided here.
				return text;
			end read_field;
			
-- 			procedure insert_component (
-- 			-- Updates a library (which is a type_components.map) by inserting a component.
-- 										   
-- 			-- The line it is about looks like:  DEF 74LS00 U 0 30 Y Y 4 F N
-- 			-- The fields meaning is as follows:
-- 			-- name, like 74LS00
-- 			-- prefix, like U
-- 			-- unknown -- CS: what is it good for ?
-- 			-- pin name position offset of supply pins, if "place pin names inside" is off. the offset assumes zero
-- 			-- show pin/pad number Y/N,
-- 			-- show pin name Y/N, -- (better port name)
-- 			-- units total, -- like 4
-- 			-- all units not interchangeable L (otherwise F), (similar to swap level in EAGLE)
-- 			-- power symbol P (otherwise N)
-- 										   
-- 			-- If the component was inserted (should be) the comp_cursor points to the component
-- 			-- for later inserting the units:
-- 				key			: in et_libraries.type_library_full_name.bounded_string;
-- 				components	: in out et_libraries.type_components.map) is
-- 
-- 				-- If only one unit provided, the flag "interchangeable" is don't care -> default swap level assumed.
-- 				-- If more units provided, the swap level is derived from field #9.
-- 				swap_level			: et_libraries.type_swap_level := et_libraries.swap_level_default;
-- 				port_name_offset	: et_libraries.type_grid;
-- 				pin_name_visible 	: et_libraries.type_pin_visible;
-- 				port_name_visible	: et_libraries.type_port_visible;
-- 			begin -- insert_component
-- 
-- 				-- For the logfile write the component name.
-- 				-- If the component contains more than one unit, write number of units.
-- 				put_line("   " & get_field_from_line(line,2)); -- 74LS00
-- 
-- 				-- Get number of units and set swap level as specified in field #9.
-- 				units_total := type_unit_id'value(get_field_from_line(line,8));
-- 				if units_total > 1 then
-- 					put_line("    with" & type_unit_id'image(units_total) & " units");
-- 
-- 					-- From the "interchangeable" flag we set the component wide swap level. It applies for 
-- 					-- all units of the component:
-- 					swap_level := to_swap_level (get_field_from_line(line,9));
-- 				end if;
-- 				
-- 				port_name_offset	:= et_libraries.type_grid'value (get_field_from_line(line,5)); -- relevant for supply pins only
-- 				pin_name_visible 	:= to_pin_visibile  (get_field_from_line(line,6));
-- 				port_name_visible	:= to_port_visibile (get_field_from_line(line,7));
-- 				
-- 				
-- 				et_libraries.type_components.insert(
-- 					container	=> components,
-- 					key			=> et_libraries.type_component_name.to_bounded_string(get_field_from_line(line,2)), -- 74LS00
-- 					position	=> comp_cursor,
-- 					inserted	=> comp_inserted,
-- 					new_item	=> (
-- 						prefix	=> et_general.type_component_prefix.to_bounded_string(get_field_from_line(line,3)), -- U
-- 						appearance => to_appearance(get_field_from_line(line,10)), -- N/P
-- 						units	=> et_libraries.type_units.empty_map
-- 						)
-- 					);
-- 
-- 				if comp_inserted then
-- 					null;
-- 				else
-- 					put_line(message_error & "line" & natural'image(line_counter) & " : component already in library !");
-- 					raise constraint_error;
-- 				end if;
-- 				
-- 			end insert_component;
-- 
-- 			procedure insert_unit (
-- 				key			: in et_libraries.type_component_name.bounded_string;
-- 				component	: in out et_libraries.type_component) is
-- 			begin
-- 				null;
-- 			end insert_unit;
			
		begin -- read_library
			put_line("  with components:");
			
			while not end_of_file loop

				-- Store line in variable "line" (see et_string_processing.ads)
				-- The schematic library files use comments (#). But only the comments at the begin
				-- of a line are relevant. Others are to be ignored. Thus test_whole_line is false.
				line := read_line(
							line => get_line,
							comment_mark => "#",
							test_whole_line => false,
							number => ada.text_io.line(current_input));
				
				case field_count(line) is
					when 0 => null; -- we skip empty lines
					when others =>

						-- Wait for component header like "DEF 74LS00 U 0 30 Y Y 4 F N".
						-- Once the header was read, the component_entered flag goes true so that
						-- no further header is expected. Once the component footer (ENDDEF) has been read,
						-- the component_entered flag goes false.
						-- The lines right after the component header are the so called "fields". The fields can be
						-- regarded as attributes (similar to EAGLE). The first four attributes are hard coded in kicad
						-- and are thus always there. In the component library the fields start with "F0" .. "Fn".
						if not component_entered then 
							
							if get_field_from_line(line,1) = et_kicad.def then
								component_entered := true;

								-- Since we are reading the fields, we set the active_section to "fields"
								active_section := fields;

								-- The commponent header provides the first component properties:
								component_name := et_libraries.type_component_name.to_bounded_string(get_field_from_line(line,2)); -- 74LS00
								
								-- for the log:
								put_line("   " & get_field_from_line(line,2)); -- 74LS00

								-- From the header we extract some basic information about the component:
								
								-- The line it is about looks like:  DEF 74LS00 U 0 30 Y Y 4 F N
								-- The fields meaning is as follows:
								-- name, like 74LS00
								-- prefix, like U
								-- unknown -- CS: what is it good for ?
								-- pin name position offset of supply pins, if "place pin names inside" is off. the offset assumes zero
								-- show pin/pad number Y/N,
								-- show pin name Y/N, -- (better port name)
								-- units total, -- like 4
								-- all units not interchangeable L (otherwise F), (similar to swap level in EAGLE)
								-- power symbol P (otherwise N)								

								prefix := et_general.type_component_prefix.to_bounded_string(get_field_from_line(line,3)); -- U
								-- CS: field #4 ?
								port_name_offset	:= et_libraries.type_grid'value (get_field_from_line(line,5)); -- relevant for supply pins only
								pin_name_visible 	:= to_pin_visibile  (get_field_from_line(line,6));
								port_name_visible	:= to_port_visibile (get_field_from_line(line,7));
								
								-- Get number of units and set swap level as specified in field #9.
								-- Swap level assumes default if only one unit available.
								units_total := type_unit_id'value(get_field_from_line(line,8));
								if units_total > 1 then
									put_line("    with" & type_unit_id'image(units_total) & " units");

									-- From the "interchangeable" flag we set the component wide swap level. It applies for 
									-- all units of the component:
									unit_swap_level := to_swap_level (get_field_from_line(line,9));
								else
									unit_swap_level := et_libraries.unit_swap_level_default;
								end if;

								-- read the appearance flag (N/P) in subfield #10
								-- This is about a component in a library -> schematic => false
								appearance := to_appearance(line => line, schematic => false);

-- 								et_libraries.type_libraries.update_element(
-- 									container	=> et_import.component_libraries,
-- 									position	=> lib_cursor,
-- 									process		=> insert_component'access);

							end if;
						else -- we are inside a component section and process subsections

							-- We wait for the end of component mark (ENDDEF) and clear the component_entered flag accordingly.
							if get_field_from_line(line,1) = et_kicad.enddef then
								component_entered := false;
							else
							-- As long as the component end mark does not appear, we process subsections as 
							-- indicated by active_section:
								case active_section is
									when fields =>
										-- Here we read the "fields". 
										-- There is no end mark for the field list. 
										-- NOTE #1: The only way to detect the end of the field list is to wait for the
										-- header of the footprint list ($FPLIST) or the header of the "draw" list (DRAW).
										-- Then the active_section is set accordingly.

										-- We wait for the header of the footprint or draw list like "$FPLIST" or "DRAW"
										-- and set active_section accordingly.
										-- As long as none of those headers occurs, we read the text fields.
										if get_field_from_line(line,1) = et_kicad.fplist then
											active_section := footprints;
										elsif get_field_from_line(line,1) = et_kicad.draw then
											active_section := draw;
										else
											-- read text fields from a component library (thats why scheamtic => false)
											case to_text_meaning(line => line, schematic => false) is

												-- If we have the reference field like "F0 "U" 0 50 50 H V C CNN"
												when et_libraries.reference =>
																
													-- Do a cross check of prefix and reference -- "U" 
													-- CS: why this redundance ? Ask the kicad makers...
													if strip_quotes(get_field_from_line(line,2)) = et_general.type_component_prefix.to_string(prefix) then
														null; -- fine
													else
														put_line(message_warning & et_string_processing.affected_line(line) & ": prefix vs. reference mismatch !");
													end if;

													reference := read_field (meaning => et_libraries.reference);
													-- for the log:
													et_libraries.write_text_properies (et_libraries.type_text(reference),4); -- actuals: text & indentation

												-- If we have a value field like "F1 "74LS00" 0 -100 50 H V C CNN"
												when et_libraries.value =>
												
													value := read_field (meaning => et_libraries.value);
													-- for the log:
													et_libraries.write_text_properies (et_libraries.type_text(value),4); -- actuals: text & indentation

												-- If we have a footprint field like "F2 "" 0 -100 50 H V C CNN"
												when et_libraries.packge =>
												
													footprint := read_field (meaning => et_libraries.packge);
													-- for the log:
													et_libraries.write_text_properies (et_libraries.type_text(footprint),4); -- actuals: text & indentation

												-- If we have a datasheet field like "F3 "" 0 -100 50 H V C CNN"
												when et_libraries.datasheet =>
												
													datasheet := read_field (meaning => et_libraries.datasheet);
													-- for the log:
													et_libraries.write_text_properies (et_libraries.type_text(datasheet),4); -- actuals: text & indentation

												-- Other mandatory fields like function and partcode are detected by F4 and F5 
												-- (not by subfield #10 !) So F4 enforces a function, F5 enforces a partcode.
												
												-- If we have a function field like "F4 "" 0 -100 50 H V C CNN" "function",
												-- we test subfield #10 against the prescribed meaning. If ok the field is read like
												-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
												when et_libraries.p_function =>
												
													if to_lower(et_libraries.text_meaning_prefix & strip_quotes(get_field_from_line(line,10))) 
															= to_lower(et_libraries.type_text_meaning'image(et_libraries.p_function)) then
																fnction := read_field (meaning => et_libraries.p_function);
																-- for the log:
																et_libraries.write_text_properies (et_libraries.type_text(fnction),4); -- actuals: text & indentation
																-- basic_text_check(fnction); -- CS
													else
														invalid_field(line);
													end if;

												-- If we have a partcode field like "F5 "" 0 -100 50 H V C CNN" "partcode",
												-- we test subfield #10 against the prescribed meaning. If ok the field is read like
												-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
												when et_libraries.partcode =>
												
													if to_lower(strip_quotes(get_field_from_line(line,10)))
															= to_lower(et_libraries.type_text_meaning'image(et_libraries.partcode)) then
																partcode := read_field (meaning => et_libraries.partcode);
																-- for the log:
																et_libraries.write_text_properies (et_libraries.type_text(partcode),4); -- actuals: text & indentation
																-- basic_text_check(partcode); -- CS
													else
														invalid_field(line);
													end if;

												-- If we have a "commissioned" field like "F6 "" 0 -100 50 H V C CNN" "commissioned",
												-- we test subfield #10 against the prescribed meaning. If ok the field is read like
												-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
												when et_libraries.commissioned =>
												
													if to_lower(strip_quotes(get_field_from_line(line,10)))
															= to_lower(et_libraries.type_text_meaning'image(et_libraries.commissioned)) then
																commissioned := read_field (meaning => et_libraries.commissioned);
																-- for the log:
																et_libraries.write_text_properies (et_libraries.type_text(commissioned),4); -- actuals: text & indentation
																-- basic_text_check(commissioned); -- CS
													else
														invalid_field(line);
													end if;

												-- If we have an "updated" field like "F7 "" 0 -100 50 H V C CNN" "updated",
												-- we test subfield #10 against the prescribed meaning. If ok the field is read like
												-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
												when et_libraries.updated =>
												
													if to_lower(strip_quotes(get_field_from_line(line,10)))
															= to_lower(et_libraries.type_text_meaning'image(et_libraries.updated)) then
																updated := read_field (meaning => et_libraries.updated);
																-- for the log:
																et_libraries.write_text_properies (et_libraries.type_text(updated),4); -- actuals: text & indentation
																-- basic_text_check(updated); -- CS
													else
														invalid_field(line);
													end if;

												-- If we have an "author" field like "F8 "" 0 -100 50 H V C CNN" "author",
												-- we test subfield #10 against the prescribed meaning. If ok the field is read like
												-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
												when et_libraries.author =>
												
													if to_lower(strip_quotes(get_field_from_line(line,10)))
															= to_lower(et_libraries.type_text_meaning'image(et_libraries.author)) then
																author := read_field (meaning => et_libraries.author);
																-- for the log:
																et_libraries.write_text_properies (et_libraries.type_text(author),4); -- actuals: text & indentation
																-- basic_text_check(author); -- CS
													else
														invalid_field(line);
													end if;

												when others => null;
													-- CS: warning about illegal fields ?
													-- CS: other text fields ?
											end case;
											
											-- CS: check appearacne vs. function vs. partcode -- see stock_manager	
										end if;

									when footprints =>
										-- Here we read the footprint list (similar to package variants in EAGLE):

										-- The list of footprints looks like this :

										-- $FPLIST
										--  Pin_Header_Straight_1X06
										--  Pin_Header_Angled_1X06
										--  Socket_Strip_Straight_1X06
										--  Socket_Strip_Angled_1X06
										-- $ENDFPLIST

										-- As long as the footer of the list ($ENDFPLIST) does not appear,
										-- we process the lines of this subsection.
										-- When the footer appears, we set active_section to "none" which means
										-- that this subsection has been processed.
										if get_field_from_line(line,1) = et_kicad.endfplist then
											active_section := none;
										else
											-- Process lines:
											put_line(to_string(line));
										end if;

									when draw =>
										-- Here we read the drawing list where lines, arcs and pins are.
										
										-- As long as the footer of the list (ENDDRAW) does not appear,
										-- we process the lines of this subsection.
										-- When the footer appears, we set active_section to "none" which means
										-- thate this subsection has been processed.
										if get_field_from_line(line,1) = et_kicad.enddraw then
											active_section := none;
										else
											-- Process lines:
											--put_line(to_string(line));
											null;
										end if;
										
									when none =>
										-- If no subsection is being processed, we wait for the "draw" header (DRAW)
										-- and set the active_section accordingly.
										-- NOTE #2: the active section "fields" is not set here but when the fields are read (see NOTE #1)
										if get_field_from_line(line,1) = et_kicad.draw then
											active_section := draw;
										end if;

								end case; -- active_section
								
							end if;
							
						end if; -- inside component section
				end case;

			end loop;
		end read_library;

		
	begin -- read_components_libraries

		-- If there were no libraries in the project file, there is nothing to do but writing a warning:
		if is_empty(et_libraries.project_libraries) then
			put_line(message_warning & " no component libraries defined in project file !");
		else
			write_message(
				file_handle => current_output,
				text => "Loading component libraries ...",
				console => true);

			-- We loop in the list of project libraries.
			while cursor /= no_element loop

				-- From lib_dir and the cursor (points to a project lib) we compose the full library file name:
				lib_file_name := to_bounded_string( compose (
								containing_directory => et_libraries.type_library_directory.to_string(et_libraries.lib_dir),
								name => et_libraries.type_library_name.to_string(element(cursor)),
								extension => file_extension_schematic_lib
								));

				write_message(
					file_handle => current_output,
					text => " " & to_string(lib_file_name),
					console => true);
				
				if exists(to_string(lib_file_name)) then
					open (
						file => et_import.library_handle,
						mode => in_file,
						name => to_string(lib_file_name));

					-- Since the libary file name is known, we insert the first empty
					-- library in the list of component libraries.
					-- After that the lib_cursor points to the latest inserted library.
					et_libraries.type_libraries.insert(
						container	=> et_import.component_libraries,
						key			=> lib_file_name,
						--new_item	=> et_libraries.type_components.empty_map,
						position	=> lib_cursor,
						inserted	=> lib_inserted
						);

					-- this is a double check. should never fail.
					if lib_inserted then
						-- Now we read the library file and add further things
						-- to the library pinted to by lib_cursor:
						set_input(et_import.library_handle);
						read_library;
					else
						put_line(message_error & to_string(lib_file_name) & " already in component libraries !");
						raise constraint_error;
					end if;
					
					close(et_import.library_handle);
				else
					put_line(message_warning & "library '" 
						& to_string(lib_file_name) 
						& "' not found !");
				end if;

				-- prepare next library file to be be read
				next(cursor);

			end loop;
		end if;
	end read_components_libraries;
	

	procedure import_design is
		use et_import.type_schematic_file_name;
		use et_libraries.type_library_directory;
		
		function read_project_file return et_import.type_schematic_file_name.bounded_string is
		-- Reads the project file in terms of LibDir and LibName. 
		-- LibDir is stored in variable lib_dir.
		-- Project library names are stored in project_libraries.
		-- Returns the name of the top level schematic file.
			line : type_fields_of_line;
			
			use et_import.type_project_file_name;
			
            section_eeschema_entered : boolean := false;
            section_eeschema_libraries_entered : boolean := false;            

            procedure clear_section_entered_flags is
            begin
                section_eeschema_entered := false;
                section_eeschema_libraries_entered := false;
            end clear_section_entered_flags;
            
		begin -- read_project_file
			put_line("reading project file ...");

			open (file => et_import.project_file_handle, mode => in_file, name => to_string(et_import.project_file_name));
			set_input (et_import.project_file_handle);
			while not end_of_file loop

				-- Save a line in variable "line" (see et_string_processing.ads)
				line := read_line(
							line => get_line,
							comment_mark => "#",
							number => ada.text_io.line(current_input),
							ifs => latin_1.equals_sign); -- fields are separated by equals sign (=)

				case field_count(line) is
					when 0 => null; -- we skip empty lines
					when 1 => -- we have a line with just one field. those lines contain headers like "[eeschema]"

						-- test header [eeschema]
						if get_field_from_line(line,1) = project_header_eeschema then
							clear_section_entered_flags;
							section_eeschema_entered := true;
						end if;

						-- test header [eeschema/libraries]
						if get_field_from_line(line,1) = project_header_eeschema_libraries then
							clear_section_entered_flags;
							section_eeschema_libraries_entered := true;
						end if;

					when 2 =>
						if section_eeschema_entered then

							-- get path to libraries (LibDir) and store it in lib_dir (see et_kicad.ads)
							-- CS: lib_dir must be a list of paths as kicad stores them like "LibDir=../../lbr;/home/tmp/.."
							-- CS: currently we assume only one path here
							if get_field_from_line(line,1) = project_keyword_library_directory then
								et_libraries.lib_dir := to_bounded_string(get_field_from_line(line,2));

								-- For the log write something like "LibDir ../../lbr"
								put_line(" " & project_keyword_library_directory & " " & to_string(et_libraries.lib_dir));
							end if;
							
						end if;

						if section_eeschema_libraries_entered then

							-- Get library names (incl. path and extension) and store them in list_of_full_library_names (see et_kicad.ads)
							-- from a line like "LibName1=bel_supply"
							-- We ignore the index of LibName. Since we store the lib names in a doubly linked list,
							-- their order remains unchanged.
							if get_field_from_line(line,1)(1..project_keyword_library_name'length) 
								= project_keyword_library_name then
								
								et_libraries.type_list_of_library_names.append(
									container => et_libraries.project_libraries, 
									new_item => et_libraries.type_library_name.to_bounded_string(
										get_field_from_line(line,2)
										--& "."
										--& file_extension_schematic_lib)); -- extension
										));

								-- For the log write something like "LibName ../../lbr/bel_connectors_and_jumpers"
								put_line(" " & get_field_from_line(line,1) 
									& " " & get_field_from_line(line,2));
							end if;

						end if;
						
					when others => null;
				end case;

				
-- 				if section_eeschema_entered or section_eeschema_libraries_entered then
-- 					put_line(" " & et_string_processing.to_string(line));
-- 				end if;
				
			end loop;
			close ( et_import.project_file_handle );

			-- Derive the schematic file name from the project file. It is just a matter of file extension.
			return et_import.type_schematic_file_name.to_bounded_string(
				compose(name => base_name(to_string(project_file_name)), 
						extension => file_extension_schematic));
		end read_project_file;

        -- While reading submodules (sheets) the path_to_submodule keeps record of current point in the design 
        -- hierarchy. Each time a submodule ABC has been found with nested submodules, the name of ABC is appended here.
        -- Once the parent module is entered again, the name ABC is removed from the list. When assigning coordinates
        -- to an object, the path_to_submodule is read. 
        -- So by concatenation of the module names of this list (from first to last) we get a full path that tells us
        -- the exact location of the module within the design hierarchy.
        path_to_submodule : type_path_to_submodule.list; -- CS: move to et_schematic ?

        -- Sometimes we need to output the location of a submodule:
        procedure write_path_to_submodule is  -- CS: move to et_schematic ?
            c : type_path_to_submodule.cursor;            
        begin
            put("path/location: ");

            c := type_path_to_submodule.first(path_to_submodule);            

			-- If there is a hierarchy deeper than 1, write path to submodule:
            if type_path_to_submodule.length(path_to_submodule) > 1 then
                for n in 1..type_path_to_submodule.length(path_to_submodule)-1 loop
                    put(type_submodule_name.to_string(type_path_to_submodule.element(c)) & ".");
                    c := type_path_to_submodule.next(c);
                end loop;
            
                c := type_path_to_submodule.last(path_to_submodule);

				-- write the submodule name
                put(type_submodule_name.to_string(type_path_to_submodule.element(c)));
			else
				-- no hierarchy. write just the submodule name
                put(type_submodule_name.to_string(type_path_to_submodule.element(c)));
            end if;
            
            new_line;            
		end write_path_to_submodule;
		
        -- Here we append a submodule name the the path_to_submodule.
        procedure append_name_of_parent_module_to_path(submodule : in type_submodule_name.bounded_string) is  -- CS: move to et_schematic ?
		begin
			--put_line("path_to_submodule: appending submodule " & type_submodule_name.to_string(submodule));
            -- Since we are dealing with file names, the extension must be removed before appending.
            type_path_to_submodule.append(path_to_submodule,
                type_submodule_name.to_bounded_string(base_name(type_submodule_name.to_string(submodule)))
                );
        end append_name_of_parent_module_to_path;

        -- Here we remove the last submodule name form the path_to_submodule.
        procedure delete_last_module_name_from_path is  -- CS: move to et_schematic ?
        begin
            type_path_to_submodule.delete_last(path_to_submodule);
        end delete_last_module_name_from_path;


        
		function read_schematic (name_of_schematic_file : in et_import.type_schematic_file_name.bounded_string) 
			return type_list_of_submodule_names_extended is
		-- Reads the given schematic file. If it contains submodules (hierarchic sheets), 
        -- they will be returned in list_of_submodules. Otherwise the returned list is empty.
        
			list_of_submodules : type_list_of_submodule_names_extended; -- list to be returned
			name_of_submodule_scratch : type_submodule_name.bounded_string; -- temporarily used before appended to list_of_submodules

			use et_string_processing;
			line : et_string_processing.type_fields_of_line;
		
			sheet_file : type_sheet_file.bounded_string;
			sheet_count_total, sheet_number_current : positive;
			net_segment_entered : boolean := false;

			-- When reading the sheet header, its content goes into sheet_header.
			-- This is stuff like:
			--     LIBS:nucleo_core-rescue
			--     LIBS:power
			--     LIBS:bel_connectors_and_jumpers
			--     LIBS:bel_primitives
			--     LIBS:bel_stm32
			--     LIBS:nucleo_core-cache
			--     EELAYER 25 0
			--     EELAYER END
			-- The library names here are of no importance. So it is sufficient to just store the 
			-- library names in the sheet header.
            sheet_header : type_sheet_header;

			schematic_headline_processed : boolean := false;
   
            
            -- When reading net labels, they are held temporarily in scratch variables, 
            -- then added to wild lists of labels for later sorting:
			simple_label_entered : boolean := false;			
			label_simple_scratch: type_net_label_simple;
			wild_simple_label_collection_scratch : type_list_of_labels_simple.vector;
			tag_label_entered : boolean := false;
			label_tag_scratch: type_net_label_tag;
            wild_tag_label_collection_scratch : type_list_of_labels_tag.vector;

            -- When reading notes, they are held temporarily in scratch variables,
            -- then added to the list of notes.
            note_entered : boolean := false;
            note_scratch : et_schematic.type_note;
			
			function to_orientation (text_in : in string) return et_general.type_orientation is
			-- Converts the label orientation to type_orientation.
			-- CS: use a dedicated type for input parameter.
				o_in : type_label_orientation := type_label_orientation'value(text_in);
				o_out : type_orientation;
			begin
				case o_in is
					when 0 => o_out := deg_180;
					when 1 => o_out := deg_90;
					when 2 => o_out := deg_0;
					when 3 => o_out := deg_270;
				end case;
				return o_out;
				-- CS: exception handler
			end to_orientation;

			function to_direction (text_in : in string) return type_label_direction is
			-- Converts the direction of a label to a type_label_direction. 
			-- CS: currently case sensitive ! Use dedicated type for input parameter.
				d_out : type_label_direction := input;
			begin
				if text_in = schematic_keyword_label_dir_input then
					d_out := input;
				elsif text_in = schematic_keyword_label_dir_output then
					d_out := output;
				elsif text_in = schematic_keyword_label_dir_bidir then
					d_out := bidir;
				elsif text_in = schematic_keyword_label_dir_tristate then
					d_out := tristate;
				elsif text_in = schematic_keyword_label_dir_passive then
					d_out := passive;
				else
					put_line(message_error & "Label direction unknown !");
					raise constraint_error;
				end if;
				
				return d_out;
			end to_direction;


			-- In the first stage, all net segments of this sheet go into a wild collection of segments.
			-- Later they will be sorted and connected by their coordinates (start and and points)
			segment_count : natural; -- holds the total number of segments within a sheet
			seg : natural; -- points to the segment being processed
			type type_segment_side is (start_point, end_point ); -- the end point of a segment
			
			type type_wild_net_segment is new type_net_segment with record
				s, e : boolean := false; -- flag indicates the end point beeing assumed
				picked : boolean := false; -- flag indicates that the segment has been added to the anonymous net
			end record;
			segment_scratch: type_wild_net_segment; -- temporarily used when reading net segments into a wild collecton of segments
			
			package type_wild_list_of_net_segments is new vectors ( 
				index_type => positive,  -- every net segment has an id
				element_type => type_wild_net_segment);
			wild_segment_collection : type_wild_list_of_net_segments.vector;

			junction_count : natural := 0;
			junction_scratch : type_net_junction; -- temporarily used when reading net junctions into a wild collection of junctions
			wild_collection_of_junctions : type_list_of_net_junctions.vector;

			function junction_sits_on_segment(junction : in type_net_junction; segment : in type_wild_net_segment) return boolean is
			-- Returns true if the given junction sits on the given net segment.
				point 		: et_schematic.type_coordinates := junction.coordinates;
				line_start 	: et_schematic.type_coordinates := segment.coordinates_start;
				line_end 	: et_schematic.type_coordinates := segment.coordinates_end;
				zero 		: constant et_libraries.type_grid := 0.0;
				sits_on_segment : boolean := false;
				d : type_distance_point_from_line;

				use et_libraries;
			begin
				-- calculate the shortes distance of point from line.
				d := distance_of_point_from_line (
					point 		=> et_libraries.type_coordinates(point),
					line_start	=> et_libraries.type_coordinates(line_start),
					line_end	=> et_libraries.type_coordinates(line_end),
					line_range	=> inside_end_points);
				
				if (not d.out_of_range) and d.distance = zero then
 					sits_on_segment := true;
 				end if;
				return sits_on_segment;
			end junction_sits_on_segment;
			
			-- Prodcedures that set the s,e or picked flag. These procedures are called via access.
			procedure set_e ( segment : in out type_wild_net_segment ) is begin segment.e := true; end set_e;
			procedure set_s ( segment : in out type_wild_net_segment ) is begin segment.s := true; end set_s;
			procedure set_picked ( segment : in out type_wild_net_segment ) is begin segment.picked := true; end set_picked;

			-- An anonymous_net is a list of net segments that are connected with each other (by their start or end points).
			-- The anonymous net gets step by step more properties specified: name, scope and some status flags:
			package type_anonymous_net is new vectors (
				index_type => positive,  -- every net segment has an id
				element_type => type_net_segment);
			type type_anonymous_net_extended is record
				segments 	: type_anonymous_net.vector;	-- the list of segments
				name 		: type_net_name.bounded_string; -- the name (derived from net labels)
				scope 		: type_scope_of_net := local;	-- the scope (derived from net labels)
				processed	: boolean := false;				-- set once a label has been found on the net
				sorted		: boolean := false;				-- set once sorted out while sorting named nets
			end record;
			-- When sorting named nets, this procedure sets the "sorted" flag of the anonymous net.
			procedure set_sorted ( anon_net : in out type_anonymous_net_extended ) is begin anon_net.sorted := true; end set_sorted;
			anonymous_net : type_anonymous_net_extended;
			
			procedure add_segment_to_anonymous_net ( id : in positive ) is
			-- Adds a net segment (indicated by id) to a list of segments connected with each other.
			-- This procedure happens to be called for a certain segment more than once (unavoidable). So the flag "picked" serves
			-- as indicator for a segment already added to the anonymous net.
				scratch : type_net_segment;
			begin
				-- If segment already picked and added to anonymous net, do nothing with this segment. Otherwise set the "picked" flag
				-- of that segment, output the coordinates of the segment, add it to anonymous net.
				if type_wild_list_of_net_segments.element(wild_segment_collection,id).picked then
					null;
					--put("  picked");
				else
					--put(et_import.report_handle,"  segment" & positive'image(id) & ":");

-- 					write_message(
-- 						file_handle => et_import.report_handle,
-- 						lf => false,
-- 						text => "segment" & positive'image(id) & ":",
-- 						identation => 3);
					
					--put("  segment" & positive'image(id) & ":");
					type_wild_list_of_net_segments.update_element(
							container => wild_segment_collection,
							index => id,
							process => set_picked'access);

					write_coordinates_of_segment(segment => type_net_segment(type_wild_list_of_net_segments.element(wild_segment_collection,id)),
												 indentation => 3);
					
					scratch := type_net_segment(type_wild_list_of_net_segments.element(wild_segment_collection,id));
					type_anonymous_net.append(anonymous_net.segments,scratch);
				end if;
			end add_segment_to_anonymous_net;

			-- The function search_for_same_coordinates returns this type:
			type type_same_coord_result is record
				valid : boolean; -- indicates that a segment with matching coordinates has been found. When false, no segment found -> consider id and side invalid
				id : positive; -- id of the segment found
				side : type_segment_side; -- end point of the segment found
			end record;
			same_coord_result : type_same_coord_result;
			side_scratch : type_segment_side;
			
			function search_for_same_coordinates (id : in positive; seg_in : in type_wild_net_segment; side : in type_segment_side) return type_same_coord_result is
			-- Starting from a segment indicated by id and the end point (given by side), 
			-- search in wild_segment_collection for a segment with matching start or end point.
			-- In general untouched segments are preferred in the search. Half processed segments are of secondary relevance.
			-- Once a suitable segment was found, sc is assigned with neccessary data to be returned to the parent unit. The search for 
			-- a suitable segment excludes fully processed segments and the given segment (id).
				sc : type_same_coord_result;
				line_start, line_end : et_schematic.type_coordinates;
				s, e	: boolean; -- indicate the end point, that has been processed already
				untouched, half_processed : boolean; -- indicate whether a segment is completely untouched or processed in only one direction

				use et_libraries;
			begin
				-- Set E/S flag:
				-- If we start the search from the end_point of a segment, the e-flag is to be set. This indicates the end_point has been processed.
				-- If we start the search from the start_point of a segment, the s-flag is to be set. This indicates the start_point has been processed.				
				case side is
					when end_point =>
						--put_line(et_import.report_handle,"  --> origin of search (END): " & type_grid'image(seg_in.coordinates_end.x) & "/" & type_grid'image(seg_in.coordinates_end.y));
						type_wild_list_of_net_segments.update_element(
								container => wild_segment_collection,
								index => id,
								process => set_e'access);
					when start_point =>
						--put_line(et_import.report_handle,"  --> origin of search (START): " & type_grid'image(seg_in.coordinates_start.x) & "/" & type_grid'image(seg_in.coordinates_start.y));
						type_wild_list_of_net_segments.update_element(
								container => wild_segment_collection,
								index => id,
								process => set_s'access);
				end case;

				-- First, search completely untouched segments (they have both e and s flag cleared).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				for i in 1..segment_count loop
					if i /= id then -- skip the given segment
						line_start := type_wild_list_of_net_segments.element(wild_segment_collection,i).coordinates_start;
						line_end   := type_wild_list_of_net_segments.element(wild_segment_collection,i).coordinates_end;
						s  := type_wild_list_of_net_segments.element(wild_segment_collection,i).s;
						e  := type_wild_list_of_net_segments.element(wild_segment_collection,i).e;
						untouched := not (s or e); -- neither s nor e set
						--fully_processed := s and e;

						if untouched then 
							--put(et_import.report_handle,"probe untouched segment: ");
							--write_coordinates_of_segment(type_net_segment(type_wild_list_of_net_segments.element(wild_segment_collection,i)));
							
							case side is
								-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
								when end_point =>
									if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
										sc.valid := true;
										sc.side := start_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

									if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
										sc.valid := true;
										sc.side := end_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

								-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.									
								when start_point =>
									if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
										sc.valid := true;
										sc.side := start_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

									if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
										sc.valid := true;
										sc.side := end_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;
							end case;
						end if;
					end if;
				end loop;

				-- No untouched segment found.
				-- Now, search half_processed segments (they have either e or s flag (BUT NOT BOTH AT THE SAME TIME!) set).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				for i in 1..segment_count loop
					if i /= id then -- skip the given segment
						line_start := type_wild_list_of_net_segments.element(wild_segment_collection,i).coordinates_start;
						line_end   := type_wild_list_of_net_segments.element(wild_segment_collection,i).coordinates_end;
						s  := type_wild_list_of_net_segments.element(wild_segment_collection,i).s;
						e  := type_wild_list_of_net_segments.element(wild_segment_collection,i).e;
						half_processed := s xor e;

						if half_processed then
							--put(et_import.report_handle,"probe half-processed segment: ");
							--write_coordinates_of_segment(type_net_segment(type_wild_list_of_net_segments.element(wild_segment_collection,i)));
							
							case side is
								-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
								when end_point =>
									if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
										sc.valid := true;
										sc.side := start_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

									if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
										sc.valid := true;
										sc.side := end_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

								-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
								when start_point =>
									if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
										sc.valid := true;
										sc.side := start_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

									if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
										sc.valid := true;
										sc.side := end_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;
							end case;
						end if;
					end if;
				end loop;
				
				sc.valid := false;
				sc.id := id;
				return sc;
			<<matching_segment_coordinates_found>>
				add_segment_to_anonymous_net(sc.id);
				--put_line(et_import.report_handle, "match");
				return sc;
			end search_for_same_coordinates;
			
			-- A list of anonymous nets:
			package type_list_of_anonymous_nets is new vectors (
				index_type => positive,  -- every anonymous net has an id
				element_type => type_anonymous_net_extended
				);
			-- The procedure add_net_to_list_of_anonymous_nets uses this container for temporarily storage of anonymous nets.
			list_of_anonymous_nets : type_list_of_anonymous_nets.vector; 
			
			procedure add_net_to_list_of_anonymous_nets is
			-- Once an anonymous net is complete, it gets appended to a list of anonymous nets. 
			-- Afterward the anonymous net is deleted. It is a vector of net segments which must be purged so that the vector
			-- "anonymous_net" can be filled with net segments of the next anonymous net.
			begin
				type_list_of_anonymous_nets.append(list_of_anonymous_nets,anonymous_net);
				type_anonymous_net.delete(anonymous_net.segments,index => 1, count => type_anonymous_net.length(anonymous_net.segments));
			end add_net_to_list_of_anonymous_nets;

			procedure associate_net_labels_with_anonymous_nets is
			-- All anonymous nets must be given a name. The name is enforced by the a net label. The first label found on the net sets the net name.
			-- Other labels on the net are checke for their name only. If the name differs from the net name set earlier, a warning is output.
			-- Nets without label remain anonymous by using the notation "N$"
				ls  :	type_net_label_simple;
				lt  : 	type_net_label_tag;				
				a,b : 	type_anonymous_net_extended;
				s   : 	type_net_segment;
				lls : 	type_list_of_labels_simple.vector;
				llt : 	type_list_of_labels_tag.vector;				
				net_scratch : type_net;
				
				function label_sits_on_segment (label : in type_net_label; segment : in type_net_segment) return boolean is
					point 		: et_schematic.type_coordinates := label.coordinates;
					line_start 	: et_schematic.type_coordinates := segment.coordinates_start;
					line_end 	: et_schematic.type_coordinates := segment.coordinates_end;
					zero 		: constant et_libraries.type_grid := 0.0;
					sits_on_segment : boolean := false;
					d : type_distance_point_from_line;

					use et_libraries;
				begin
					-- calculate the shortes distance of point from line.
					d := distance_of_point_from_line (
						point		=> et_libraries.type_coordinates(point),
						line_start	=> et_libraries.type_coordinates(line_start),
						line_end	=> et_libraries.type_coordinates(line_end),
						line_range	=> with_end_points);
					--put_line(et_import.report_handle,"distance: " & type_grid'image(d.distance));
					if not d.out_of_range and d.distance = zero then
						sits_on_segment := true;
					end if;
					return sits_on_segment;
				end label_sits_on_segment;
				
			begin -- associate_net_labels_with_anonymous_nets
				-- This does only make sense if there are nets at all:
				if type_list_of_anonymous_nets.length(list_of_anonymous_nets) > 0 then
					--put_line(et_import.report_handle,"associating net labels with nets ...");
					write_message(
						file_handle => et_import.report_handle,
						text => "associating net labels with nets ...",
						identation => 2);
					
					-- Loop in list of anonymous nets, get a (non-processed-yet) net, loop in list of segments and find a (non-processed-yet)
					-- net label that sits on the net segment. If label sits on segment:
					--  - assume label text as name of net (and check other labels of the anonymous net)
					--
					--  - mark label as processed
					--  - update/replace label in wild_simple_label_collection_scratch or wild_tag_label_collection_scratch
					--
					--  - Collect label in temporarily list of labels.
					--
					--  - Mark anonymous net as processed. This indicates that the net has a name (given by a label).
					--    Non-Processed nets are those without a label.
					--  - update/replace anonymous net in list_of_anonymous_nets
					for n in 1..type_list_of_anonymous_nets.length(list_of_anonymous_nets) loop
						a := type_list_of_anonymous_nets.element(list_of_anonymous_nets, positive(n)); -- get anonymous net
						--put_line(et_import.report_handle,"anonymous net #" & trim(count_type'image(n),left) & ": ");
						if not a.processed then
							for b in 1..type_anonymous_net.length(a.segments) loop -- loop for each segment of anonymous_net
								s := type_anonymous_net.element(a.segments, positive(b)); -- get segment

								--put(et_import.report_handle, "segment: "); write_coordinates_of_segment(s);
								
								-- Loop in list of simple labels:
								if type_list_of_labels_simple.length(wild_simple_label_collection_scratch) > 0 then -- do that if there are simple labels at all
									--put_line(" simple labels ...");
									for l in 1..type_list_of_labels_simple.length(wild_simple_label_collection_scratch) loop 
										ls := type_list_of_labels_simple.element(wild_simple_label_collection_scratch, positive(l)); -- get simple label
										if not ls.processed then
											--put(et_import.report_handle, "   probing "); write_coordinates_of_label( type_net_label(ls));
											if label_sits_on_segment(label => type_net_label(ls), segment => s) then

												--put(et_import.report_handle, "match: "); write_coordinates_of_label( type_net_label(ls));

												-- The first matching label dictates the net name. 
												-- If other labels with text differing from net name found, output warning.
												if type_net_name.length(a.name) = 0 then -- If this is the first matching label
													a.name := ls.text; -- assume the label text as net name.
												else
													-- If label text is different from previously assigned net name:
													if type_net_name.to_string(a.name) /= type_net_name.to_string(ls.text) then 
														put_line(et_import.report_handle,message_warning & "Net '" & type_net_name.to_string(a.name) &
														"' has contradicting label '" & type_net_name.to_string(ls.text) & "'");
													end if;
												end if;

												-- mark simple label as processed and update/replace it in wild_simple_label_collection_scratch
												ls.processed := true;
												type_list_of_labels_simple.replace_element(
													container => wild_simple_label_collection_scratch,
													index => positive(l),
													new_item => ls);

												-- Collect simple label (ls) in temporarily list of simple labels (lls).
												type_list_of_labels_simple.append(lls,ls);

												-- Mark anonymous net as processed.												
												a.processed := true;
											end if;
										end if;
									end loop;

									-- Copy list of simple labels (lls) to current segment (s).
									s.label_list_simple := lls;
									-- Update/replace segment in current anonymous net.
									type_anonymous_net.replace_element(
										container => a.segments, -- the list of segments of the current anonymous net
										index => positive(b), -- the segment id
										new_item => s); -- the updated segment
									-- Clean up: Purge temporarily list of simple labels for next spin.
									type_list_of_labels_simple.delete(container => lls, index => 1, count => type_list_of_labels_simple.length(lls));

									-- Update/replace anonymous net in list_of_anonymous_nets.
									type_list_of_anonymous_nets.replace_element(
										container => list_of_anonymous_nets, -- the list of anonymous nets
										index => positive(n), -- the anonymous net id
										new_item => a); -- the updated anonymous net
								end if;
								
								-- Loop in list of tag labels:
								if type_list_of_labels_tag.length(wild_tag_label_collection_scratch) > 0 then -- do that if there are tag labels at all
									--put_line(" hierarchic and global labels ...");									
									for l in 1..type_list_of_labels_tag.length(wild_tag_label_collection_scratch) loop 
										lt := type_list_of_labels_tag.element(wild_tag_label_collection_scratch, positive(l)); -- get tag label
										if not lt.processed then								
											if label_sits_on_segment(label => type_net_label(lt), segment => s) then

-- 												put_line(et_import.report_handle," tag label: " & type_net_name.to_string(lt.text) & " position:" &
-- 												type_grid'image(lt.coordinates.x) & "/" &
-- 												trim(type_grid'image(lt.coordinates.y),left));

												write_label_properties(type_net_label(lt));
-- 												write_message(
-- 													file_handle => et_import.report_handle,
-- 													text => "tag label: " & type_net_name.to_string(lt.text) & " position:" &
-- 															type_grid'image(lt.coordinates.x) & "/" &
-- 															trim(type_grid'image(lt.coordinates.y),left),
-- 													identation => 3);

												-- The first matching label dictates the net name and scope. 
												-- If other labels with text differing from net name found, output warning.
												if type_net_name.length(a.name) = 0 then -- If this is the first matching label
													a.name := lt.text; -- assume the label text as net name.
													if lt.global then 
														a.scope := global;
													end if;
													if lt.hierarchic then 
														a.scope := hierarchic;
													end if;
												else
													-- If label text is different from previously assigned net name:
													if type_net_name.to_string(a.name) /= type_net_name.to_string(lt.text) then 
														put_line(et_import.report_handle,message_warning & "Net '" & type_net_name.to_string(a.name) &
														"' has contradicting label '" & type_net_name.to_string(lt.text) & "'");
													end if;

													-- CS: check for contradicting scope
												end if;

												-- mark tag label as processed and update/replace it in wild_tag_label_collection_scratch
												lt.processed := true;
												type_list_of_labels_tag.replace_element(
													container => wild_tag_label_collection_scratch,
													index => positive(l),
													new_item => lt);

												-- Collect tag label (lt) in temporarily list of simple labels (llt).
												type_list_of_labels_tag.append(llt,lt);

												-- Mark anonymous net as processed.												
												a.processed := true;
											end if;
										end if;
									end loop;

									-- Copy list of tag labels (llt) to current segment (s).
									s.label_list_tag := llt;
									-- Update/replace segment in current anonymous net.
									type_anonymous_net.replace_element(
										container => a.segments, -- the list of segments of the current anonymous net
										index => positive(b), -- the segment id
										new_item => s); -- the updated segment
									-- Clean up: Purge temporarily list of tag labels for next spin.
									type_list_of_labels_tag.delete(container => llt, index => 1, count => type_list_of_labels_tag.length(llt));

									-- Update/replace anonymous net in list_of_anonymous_nets.
									type_list_of_anonymous_nets.replace_element(
										container => list_of_anonymous_nets, -- the list of anonymous nets
										index => positive(n), -- the anonymous net id
										new_item => a); -- the updated anonymous net
								end if;
							end loop;
						end if;
					end loop;

					-- Sort anonymous nets without label.
					-- Anonymous nets without label have no name -> "processed" flag is still cleared.
					-- As placeholder for the name we use the notation "N$" where n is an index (derived from the element id in list_of_anonymous_nets)
					-- Their scope is strictly "local".
					-- We us an intermediate variable net_scratch for transfer to the module netlist.
					write_message(
						file_handle => et_import.report_handle,
						text => "sorting name-less nets ...",
						identation => 2);
					
					for n in 1..type_list_of_anonymous_nets.length(list_of_anonymous_nets) loop
						a := type_list_of_anonymous_nets.element(list_of_anonymous_nets, positive(n)); -- get anonymous net
						if not a.processed then

							-- build temporarily net
							net_scratch.name := type_net_name.to_bounded_string("N$" & trim(count_type'image(n),left));
							write_message(
								file_handle => et_import.report_handle,
								text => type_net_name.to_string(net_scratch.name),
								identation => 3);
							
							net_scratch.scope := local;

							-- append segments to net_scratch
							for b in 1..type_anonymous_net.length(a.segments) loop -- loop for each segment of anonymous_net a
								s := type_anonymous_net.element(a.segments, positive(b)); -- get segment
								type_list_of_net_segments.append(container => net_scratch.segments, new_item => s);
								write_coordinates_of_segment(segment => s, indentation => 4);
							end loop;

                            -- assign coordinates
                            net_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                            net_scratch.coordinates.path := path_to_submodule;
                            -- CS: x,y coordinates should be the lowest available on the first sheet.
                            -- CS: do not assign sheet and x/y at all ?
                            -- net_scratch.coordinates.sheet := sheet_number_current;
                            
							-- append net_scratch to module netlist, then purge net_scratch.segments for next spin
							type_net_list_of_module.append(container => module.nets, new_item => net_scratch);
							type_list_of_net_segments.delete(
								container => net_scratch.segments,
								index => 1,
								count => type_list_of_net_segments.length(net_scratch.segments));
						end if;
					end loop;

					-- Sort anonymous nets with label.
					write_message(
						file_handle => et_import.report_handle,
						text => "sorting named nets ...",
						identation => 2);
					
					for n in 1..type_list_of_anonymous_nets.length(list_of_anonymous_nets) loop
						a := type_list_of_anonymous_nets.element(list_of_anonymous_nets, positive(n)); -- get anonymous net
						if a.processed and not a.sorted then -- if it has not been sorted yet
							--put(et_import.report_handle," " & type_net_name.to_string(a.name));
							write_message(
								file_handle => et_import.report_handle,
								text => type_net_name.to_string(a.name),
								identation => 3,
								lf => false);
							
							net_scratch.name := a.name;
							net_scratch.scope := a.scope;
							--put_line(et_import.report_handle," is " & type_scope_of_net'image(net_scratch.scope) & " with segments:");
							write_message(
								file_handle => et_import.report_handle,
								text => " is " & type_scope_of_net'image(net_scratch.scope) & " with segments:");

							-- append segments to net_scratch
							for b in 1..type_anonymous_net.length(a.segments) loop -- loop for each segment of anonymous_net a
								s := type_anonymous_net.element(a.segments, positive(b)); -- get segment
								type_list_of_net_segments.append(container => net_scratch.segments, new_item => s);
								write_coordinates_of_segment(segment => s, indentation => 4);
							end loop;

							-- Look for other anonymous nets with the same name (a.name). Start searching from position n+1:
							-- Mark anonymous net as "sorted".
							-- If last anonymous net reached, do not look for other nets with same name.
							if n = type_list_of_anonymous_nets.length(list_of_anonymous_nets) then -- last net reached
								null; 
							else -- search for nets with same name
								for o in n+1..type_list_of_anonymous_nets.length(list_of_anonymous_nets) loop
									b := type_list_of_anonymous_nets.element(list_of_anonymous_nets, positive(o)); -- get anonymous net
									if b.processed then
										if type_net_name.to_string(b.name) = type_net_name.to_string(a.name) then

											-- CS: make sure scope of the anonymous net is the same

											-- append segments to net_scratch
											for c in 1..type_anonymous_net.length(b.segments) loop -- loop for each segment of anonymous_net b
												s := type_anonymous_net.element(b.segments, positive(c)); -- get segment
												type_list_of_net_segments.append(container => net_scratch.segments, new_item => s);
												write_coordinates_of_segment(segment => s, indentation => 4);
											end loop;

											-- mark anonymous net as "sorted" so that the outer loop can skip it in further spins
											type_list_of_anonymous_nets.update_element(
												container => list_of_anonymous_nets, 
												index => positive(o), 
												process => set_sorted'access);
										end if;
									end if;
								end loop;
							end if;

                            -- assign coordinates
                            net_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                            net_scratch.coordinates.path := path_to_submodule;
                            -- CS: x,y coordinates should be the lowest available on the first sheet.
                            -- CS: do not assign sheet and x/y at all ?
                            -- net_scratch.coordinates.sheet := sheet_number_current;
                            
							-- append net_scratch to module netlist, then purge net_scratch.segments for next spin
							type_net_list_of_module.append(container => module.nets, new_item => net_scratch);
							type_list_of_net_segments.delete(
								container => net_scratch.segments,
								index => 1,
								count => type_list_of_net_segments.length(net_scratch.segments));

 						end if;
					end loop;
					
				else
					--put_line(et_import.report_handle,message_warning & "The schematic contains no nets to associate labels with !");
					write_message(
						file_handle => et_import.report_handle,
						text => message_warning & "The schematic does not contain nets to associate net labels with !",
						identation => 2);
				end if;
			end associate_net_labels_with_anonymous_nets;
			
			procedure process_junctions is
			-- Breaks down all net segments where a junction sits on. In the end, the number of net segments increases.
				
			-- Loops in type_wild_list_of_net_segments, tests if a junction sits on a segment.
			-- Then splits the segment where the junction sits. If there are junctions left on the remaining fragments,
			-- they will be detected in the next spin. 
			-- The flag segment_smashed indicates there are no more segments left with a junction.
				segment_scratch : type_wild_net_segment;
				junction_scratch : type_net_junction;
				
				procedure change_segment_start_coordinates ( segment : in out type_wild_net_segment) is 
				begin
					segment.coordinates_start := junction_scratch.coordinates;
				end change_segment_start_coordinates;
				
				segment_smashed : boolean := true; -- indicates whether a segment has been broken down
            begin -- process junctions
				-- Break down net segments that have a junction. Do that if the sheet has junctions at all. Otherwise skip this procedure.
				-- After breaking down net segments, the numbner of segments increases, so segment_count must be updated finally.
				if junction_count > 0 then
					write_message(
						file_handle => et_import.report_handle,
						text => "processing" & natural'image(junction_count) & " net junctions ...",
						identation => 2
						);

					-- We reason there are segments to be broken down. After smashing a segment, segment_count increases. If it
					-- does not increase anymore, all segments are processed.
					while segment_smashed loop
					
						loop_s:
						for s in 1..segment_count loop
							segment_scratch := type_wild_list_of_net_segments.element(wild_segment_collection,s); -- get a segment
							--put_line(et_import.report_handle,natural'image(s));

							-- loop in junction list until a junction has been found that sits on the segment
							for j in 1..junction_count loop 
								junction_scratch := type_list_of_net_junctions.element(wild_collection_of_junctions,j);
								if junction_sits_on_segment(junction => junction_scratch, segment => segment_scratch) then -- match

									--write_coordinates_of_segment (type_net_segment(segment_scratch));
									write_coordinates_of_junction (junction_scratch, 3); -- last actual is indentation

									-- move start coord. of the current segment to the position of the junction
									type_wild_list_of_net_segments.update_element(
										container => wild_segment_collection,
										index => s,
										process => change_segment_start_coordinates'access
										);

									-- replace end coord. of segment_scratch by pos. of junction
									segment_scratch.coordinates_end   := junction_scratch.coordinates;
									type_wild_list_of_net_segments.append(
										container => wild_segment_collection,
										new_item => segment_scratch
										);

									exit loop_s;
								end if;
							end loop;
						end loop loop_s;

						-- Test if segment_count has increased. If yes, set segment_smashed flag so that the wild_segment_collection
						-- can be searched again. Otherwise clear segment_scratch which ends this procedure.
						if natural(type_wild_list_of_net_segments.length(wild_segment_collection)) > segment_count then
							segment_smashed := true;
							-- update segment_count (should increment by 1)
							segment_count := natural(type_wild_list_of_net_segments.length(wild_segment_collection));
						else
							segment_smashed := false;							
						end if;
					end loop;
					
					write_message(
						file_handle => et_import.report_handle,
						text => "update: net segments total: " & natural'image(segment_count),
						identation => 2);
				end if;
			end process_junctions;


			description_entered : boolean := false;
            description_processed : boolean := false;
            sheet_description_entered : boolean := false;

            -- When reading the sheet descripton we need a temporarily places for storage. They will
            -- later be appended to the main module.
            drawing_frame_scratch : type_frame; -- a single drawing frame
            title_block_text_scratch : type_title_block_text; -- a single text within the title block
            list_of_title_block_texts_scratch : type_list_of_title_block_texts.vector; -- a list of title block texts
            title_block_scratch : type_title_block; -- a full title block

            -- When reading gui submodules (kicad refers to them as "sheets") they are stored temporarily here.
            -- This temporarily variable needs defaults in order to prevent misleading compiler warnings.
            submodule_gui_scratch : type_gui_submodule := (
                name => type_submodule_name.to_bounded_string(""),
                text_size_of_name => 58,
                text_size_of_file => 58,                
                coordinates => (    path => path_to_submodule,
                                    module_name => type_submodule_name.to_bounded_string( to_string(name_of_schematic_file)),
                                    sheet_number => 1,
                                    x => 0.0,
                                    y => 0.0 
                               ),
                size_x => 0.0,
                size_y => 0.0,
                timestamp => "00000000"
                );

            
			-- This is relevant for reading components:
			component_entered : boolean := false; -- indicates that a component is being read

			-- These temporarily used variables store information used when assembling and inserting a component
			-- or a unit in the component/unit list:
			tmp_component_name_in_lib	: et_libraries.type_component_name.bounded_string;
			tmp_component_appearance	: et_general.type_component_appearance := et_general.sch;
			tmp_component_reference		: et_general.type_component_reference;
			tmp_component_unit_name		: et_libraries.type_unit_name.bounded_string;
			tmp_component_alt_repres	: et_schematic.type_alternative_representation;
			tmp_component_timestamp		: et_general.type_timestamp;
			tmp_component_position		: et_schematic.type_coordinates;

            tmp_component_text_reference	: et_libraries.type_text (meaning => et_libraries.reference);
            tmp_component_text_value		: et_libraries.type_text (meaning => et_libraries.value);
            tmp_component_text_commissioned : et_libraries.type_text (meaning => et_libraries.commissioned);
            tmp_component_text_updated		: et_libraries.type_text (meaning => et_libraries.updated);
            tmp_component_text_author		: et_libraries.type_text (meaning => et_libraries.author);
			tmp_component_text_packge		: et_libraries.type_text (meaning => et_libraries.packge); -- like "SOT23"
			tmp_component_text_datasheet	: et_libraries.type_text (meaning => et_libraries.datasheet); -- might be useful for some special components
			tmp_component_text_fnction		: et_libraries.type_text (meaning => et_libraries.p_function); -- to be filled in schematic later by the user
			tmp_component_text_partcode		: et_libraries.type_text (meaning => et_libraries.partcode); -- like "R_PAC_S_0805_VAL_"			

			-- These are the "field found" flags. They signal if a particular text field has been found.
			-- They are cleared by procdure "init_temp_variables" once a new compoenent is entered.
			-- They are evaluated when a component section is left.
            tmp_component_text_reference_found		: boolean;
            tmp_component_text_value_found			: boolean;
            tmp_component_text_commissioned_found	: boolean;
            tmp_component_text_updated_found		: boolean;
            tmp_component_text_author_found			: boolean;
			tmp_component_text_packge_found			: boolean;
			tmp_component_text_datasheet_found		: boolean;
			tmp_component_text_fnction_found		: boolean;
			tmp_component_text_partcode_found		: boolean;
			
			
			procedure init_temp_variables is
			begin
				-- clear "field found" flags
				tmp_component_text_reference_found		:= false;
				tmp_component_text_value_found			:= false;
				tmp_component_text_commissioned_found	:= false;
				tmp_component_text_updated_found		:= false;
				tmp_component_text_author_found			:= false;
				tmp_component_text_packge_found			:= false;
				tmp_component_text_datasheet_found		:= false;
				tmp_component_text_fnction_found		:= false;
				tmp_component_text_partcode_found		:= false;
				
				tmp_component_text_packge.content := et_libraries.type_text_content.to_bounded_string("");
				-- CS: init text properties ?
				-- CS: init remaining tmp vars ?

			end init_temp_variables;

			procedure check_text_fields is
			-- Tests if a "field found" flag is cleared and raises an alarm in that case.
			-- Perfoms a plausibility and symtax check on the text fields.
				procedure missing_field ( m : in et_libraries.type_text_meaning) is 
				begin
					write_message (
						file_handle => current_output,
						text => message_error 
							& "component " & et_general.to_string (tmp_component_reference) 
							& latin_1.space
							& et_schematic.to_string(tmp_component_position)
							& latin_1.lf
							& "text field '" & et_libraries.to_string(m) & "' missing !",
						console => true);
					
					raise constraint_error;
				end missing_field;
			begin
				-- reference
				if not tmp_component_text_reference_found then
					missing_field (et_libraries.reference);
				else
					null; -- CS: verify tmp_component_text_reference equals tmp_component_reference
				end if;

				-- value
				if not tmp_component_text_value_found then
					missing_field (et_libraries.value);
				else
					null;
					-- CS: check content of tmp_component_text_value
				end if;

				-- commissioned
				if not tmp_component_text_commissioned_found then
					missing_field (et_libraries.commissioned);
				else
					null;
					-- CS: check content of tmp_component_text_commissioned
				end if;

				-- updated
				if not tmp_component_text_updated_found then
					missing_field (et_libraries.updated);
				else
					null;
					-- CS: check content of tmp_component_text_updated
				end if;

				-- author
				if not tmp_component_text_author_found then
					missing_field (et_libraries.author);
				else
					null;
					-- CS: check content of tmp_component_text_author
				end if;

				-- If we are checking fields of a real component there are more 
				-- fields to be checked. If it is about a virtual component, those 
				-- fields are ignored and thus NOT checked:
				case tmp_component_appearance is
					when sch_pcb =>
							
						-- package
						if not tmp_component_text_packge_found then
							missing_field (et_libraries.packge);
						else
							null;
							-- CS: check content of tmp_component_text_packge
						end if;

						-- datasheet
						if not tmp_component_text_datasheet_found then
							missing_field (et_libraries.datasheet);
						else
							null;
							-- CS: check content of tmp_component_text_datasheet
						end if;

						-- partcode
						if not tmp_component_text_partcode_found then
							missing_field (et_libraries.partcode);
						else
							null;
							-- CS: check content of tmp_component_text_partcode
						end if;
						
						-- purpose
						if not tmp_component_text_fnction_found then
							missing_field (et_libraries.p_function);
						else
							null;
							-- CS: check content of tmp_component_text_fnction
						end if;

					when others => null; -- CS ?
				end case;
				
			end check_text_fields;
			

			function to_text return et_libraries.type_text is
			-- Converts a field like "F 1 "green" H 2700 2750 50  0000 C CNN" to a type_text
				function field ( line : in type_fields_of_line; position : in positive) return string renames get_field_from_line;
			begin
				return (
					-- read text field meaning
					meaning 	=> to_text_meaning(line => line, schematic => true),

					-- read content like "N701" or "NetChanger" from field position 3
					content		=> et_libraries.type_text_content.to_bounded_string (strip_quotes(field(line,3))),

					-- read orientation like "H" -- type_schematic_field_orientation
					orientation	=> to_text_orientation (field(line,4)),

					-- read coordinates
					position	=> (x => et_libraries.type_grid'value(field(line,5)),
									y => et_libraries.type_grid'value(field(line,6))),
					size		=> et_libraries.type_text_size'value (field(line,7)),
					style		=> to_text_style (style_in => field(line,10), text => false),
					line_width	=> 0, -- not provided here -- CS: define a default ?

					-- build text visibility
					visible		=> to_field_visible (
										vis_in		=> field(line,8),
										schematic	=> true),

					-- build text alignment
					alignment	=> (
									horizontal	=> to_alignment_horizontal (field(line,9)),
									vertical	=> to_alignment_vertical   (field(line,10)))
					);
			end to_text;

			
			component_cursor	: type_components.cursor; -- points to a component of the module
			component_inserted	: boolean; -- used when a component is being inserted into the component list of a module
			
-- 			procedure fetch_components_from_library is
			-- This procedure looks up the sheet_header and reads the library names stored there.
			-- The full library names (incl. containing directory) are build.
			-- The libraries are then read and theri content added to the sheet_header.
-- 				use type_component_libraries;
-- 				use et_general.type_library_full_name;
-- 				lib_cursor : type_component_libraries.cursor := first(sheet_header.libraries);
-- 				lib_file : et_general.type_library_full_name.bounded_string;
-- 			begin
-- 				put_line("  loading component libraries ...");
-- 				if not is_empty(sheet_header.libraries) then
-- 					while lib_cursor /= type_component_libraries.no_element loop
-- 						lib_file := to_bounded_string(compose(
-- 							containing_directory => et_general.type_library_directory.to_string(lib_dir),
-- 							name => et_general.type_library_name.to_string(key(lib_cursor)),
-- 							extension => file_extension_schematic_lib
-- 							));
-- 						put_line("   " & to_string(lib_file));
-- 						next(lib_cursor);
-- 					end loop;
-- 				end if;	
-- 			end fetch_components_from_library;

			procedure insert_component is
			-- Inserts the component in the component list of the module (module.components).
			-- Components may occur multiple times, which implies they are
			-- split into units (EAGLE refers to them as "gates").
			-- Only the first occurence of the component leads to appending it to the component list of the module.
			
			-- The component to be inserted gets assembled with the temporarily variables assigned until now.
			-- Tests if a footprint has been associated with the component.

				-- This is required as scratch variable when breaking down the content of the footprint content.
				-- Kicad saves library and footprint name in a string like "bel_opto:LED_S_0805" separated by colon.
				-- When a real component (appearance sch_pcb) is inserted, this variable is loaded with
				-- the library name and the footprint.
				tmp_library_footprint : et_string_processing.type_fields_of_line;

				function field (
					line		: in et_string_processing.type_fields_of_line;
					position	: in positive) return string renames et_string_processing.get_field_from_line;

			begin -- insert_component
				-- The compoenent is inserted into the components list of the module according to its appearance.
				-- If the component has already been inserted, it will not be inserted again.
				case tmp_component_appearance is
					
					when sch => -- we have a line like "L P3V3 #PWR07"
				
						type_components.insert(
							container => module.components,
													
							key => tmp_component_reference,

							new_item => (
								appearance => et_general.sch, -- the component appears in schematic only
								name_in_library => tmp_component_name_in_lib,
								value => et_schematic.type_component_value.to_bounded_string (field_content(tmp_component_text_value)),
								commissioned => et_string_processing.type_date (field_content(tmp_component_text_commissioned)),
								updated => et_string_processing.type_date (field_content(tmp_component_text_updated)),
								author => et_general.type_person_name.to_bounded_string (field_content(tmp_component_text_author)),

								-- At this stage we do not know if and how many units there are. So the unit list is empty.
								units => et_schematic.type_units.empty_map),
							
							position => component_cursor,
							inserted => component_inserted); -- this flag is just formal. no further evaluation

							et_schematic.write_component_properties ( component => component_cursor, indentation => 2);

							-- CS: Test value ?
							
					when sch_pcb => -- we have a line like "L 74LS00 U1"

						-- break down the footprint content like "bel_opto:LED_S_0805".
						tmp_library_footprint := et_string_processing.read_line(
								line => field_content (tmp_component_text_packge),
								ifs => latin_1.colon);

						
						type_components.insert(
							container => module.components,

							key => tmp_component_reference,

							new_item => (
								appearance => et_general.sch_pcb, -- the component appears in both schematic and layout
								name_in_library => tmp_component_name_in_lib,
								value => et_schematic.type_component_value.to_bounded_string(field_content(tmp_component_text_value)),
								commissioned => et_string_processing.type_date (field_content(tmp_component_text_commissioned)),
								updated => et_string_processing.type_date (field_content(tmp_component_text_updated)),
								author => et_general.type_person_name.to_bounded_string (field_content(tmp_component_text_author)),

								-- properties of a real component (appears in schematic and layout);
								datasheet => et_libraries.type_component_datasheet.to_bounded_string (field_content(tmp_component_text_datasheet)),
								partcode => et_libraries.type_component_partcode.to_bounded_string (field_content(tmp_component_text_partcode)),
								purpose => et_libraries.type_component_purpose.to_bounded_string (field_content(tmp_component_text_fnction)),
								
								-- Assemble the package variant.
								-- NOTE: There is no way to identifiy the name of the package variant like TL084D or TL084N.
								-- For this reason we leave the variant name empty.
								variant =>
									( 
									variant => (

										-- get the package name from the footprint field 
										packge => 
											et_libraries.type_component_package_name.to_bounded_string(
												field(line => tmp_library_footprint, position => 2)),

										-- get the library file name from the footpint field
										library => et_libraries.type_library_full_name.to_bounded_string(
												field(line => tmp_library_footprint, position => 1))),

									-- The variant name is left empty.
									name => et_libraries.type_component_variant_name.to_bounded_string("")
									),

								-- At this stage we do not know if and how many units there are. So the unit list is empty for the moment.
								units => et_schematic.type_units.empty_map),
							
							position => component_cursor,
							inserted => component_inserted); -- this flag is just formal. no further evaluation

							et_schematic.write_component_properties ( component => component_cursor, indentation => 2);

							-- Test if footprint has been associated with the component.
							if field_content (tmp_component_text_packge)'size = 0 then
								write_message(
									file_handle => current_output,
									text => message_error & et_general.to_string(tmp_component_reference) & ": footprint not specified !",
									console => true);
								raise constraint_error;
							end if;

							-- The libaray and footpint name could be tested separately.
-- 							-- Test footprint contains a libaray name. example: "bel_opto:LED_S_0805"
-- 							if field(line => tmp_library_footprint, position => 1)'size = 0 then
-- 								write_message(
-- 									file_handle => current_output,
-- 									text => message_error & et_general.to_string(tmp_component_reference) & ": footprint library not specified !",
-- 									console => true);
-- 								raise constraint_error;
-- 							end if;
-- 
-- 							-- Test if footprint has been associated with the component.
-- 							if field(line => tmp_library_footprint, position => 2)'size = 0 then
-- 								write_message(
-- 									file_handle => current_output,
-- 									text => message_error & et_general.to_string(tmp_component_reference) & ": footprint not specified !",
-- 									console => true);
-- 								raise constraint_error;
-- 							end if;

							-- CS: verify reference against tmp_component_reference. should be equal. @kicad: why this redundance ?
							-- CS: Test value
							-- CS: test partcode, verify agsinst prefix, value and package
							-- CS: test function against prefix of user interactive parts (X, SW, LED, ...)
						
					when others => -- CS: This should never happen. A subtype of type_component_appearance could be a solution.
						null;
						raise constraint_error;
						
				end case;
				
			end insert_component;
			

			procedure insert_unit ( key : in et_general.type_component_reference; component : in out et_schematic.type_component ) is 
			-- Inserts a unit into the unit list of a component. The text fields around a unit are placeholders.
			-- The properties of the placeholder texts are loaded with the properties of the text fields of the units
			-- found in the schematic. The idea behind is to store just basic text properties (type_text_basic) 
			-- for the texts around the unit, but not its content. The content is stored with the component as a kind
			-- of meta-data. See procedure insert_component.
			-- Raises constraint error if unit already in unit list of component.

				unit_cursor : type_units.cursor;
				unit_inserted : boolean;

			begin
				case tmp_component_appearance is

					when sch =>

						et_schematic.type_units.insert(
							container => component.units, -- the unit list of the component
							new_item => (
								appearance		=> et_general.sch,
								position		=> tmp_component_position,
								name			=> tmp_component_unit_name,
								timestamp		=> tmp_component_timestamp,
								alt_repres		=> tmp_component_alt_repres,

								-- placeholders:
								-- Convert tmp_component_text_* to a placeholder while maintaining the text meaning.
								reference		=> ( et_libraries.type_text_basic (tmp_component_text_reference)
													with meaning => tmp_component_text_reference.meaning ),
								value			=> ( et_libraries.type_text_basic (tmp_component_text_value)
													with meaning => tmp_component_text_value.meaning ),
								updated			=> ( et_libraries.type_text_basic (tmp_component_text_updated)
													with meaning => tmp_component_text_updated.meaning ),
								author			=> ( et_libraries.type_text_basic (tmp_component_text_author)
													with meaning => tmp_component_text_author.meaning ),
								commissioned	=>  ( et_libraries.type_text_basic (tmp_component_text_commissioned)
													with meaning => tmp_component_text_commissioned.meaning )
								),

							position => unit_cursor,
							inserted => unit_inserted,

							key => tmp_component_unit_name); -- the unit name

					
					when sch_pcb =>
			
						et_schematic.type_units.insert(
							container => component.units, -- the unit list of the component
							new_item => (
								appearance		=> et_general.sch_pcb,
								position		=> tmp_component_position,
								name			=> tmp_component_unit_name,
								timestamp		=> tmp_component_timestamp,
								alt_repres		=> tmp_component_alt_repres,

								-- placeholders:
								-- Convert tmp_component_text_* to a placeholder while maintaining the text meaning.
								reference		=> ( et_libraries.type_text_basic (tmp_component_text_reference)
													with meaning => tmp_component_text_reference.meaning ),
								value			=> ( et_libraries.type_text_basic (tmp_component_text_value)
													with meaning => tmp_component_text_value.meaning ),
								packge			=> ( et_libraries.type_text_basic (tmp_component_text_packge)
													with meaning => tmp_component_text_packge.meaning ),
								datasheet		=> ( et_libraries.type_text_basic (tmp_component_text_datasheet)
													with meaning => tmp_component_text_datasheet.meaning ),
								fnction			=> ( et_libraries.type_text_basic (tmp_component_text_fnction)
													with meaning => tmp_component_text_fnction.meaning ),
								partcode		=> ( et_libraries.type_text_basic (tmp_component_text_partcode)
													with meaning => tmp_component_text_partcode.meaning ),
								updated			=> ( et_libraries.type_text_basic (tmp_component_text_updated)
													with meaning => tmp_component_text_updated.meaning ),
								author			=> ( et_libraries.type_text_basic (tmp_component_text_author)
													with meaning => tmp_component_text_author.meaning ),
								commissioned	=>  ( et_libraries.type_text_basic (tmp_component_text_commissioned)
													with meaning => tmp_component_text_commissioned.meaning )
								),

							position => unit_cursor,
							inserted => unit_inserted,

							key => tmp_component_unit_name); -- the unit name

					when others => null; -- CS
				end case;
					
				-- If unit alread in list, raise alarm and abort.
				if not unit_inserted then
					write_message(
						file_handle => current_output,
						text => message_error & "multiple occurence of the same unit !",
						console => true);
					raise constraint_error;
				end if;
					
				write_unit_properties ( unit => unit_cursor, indentation => 3 );
					
			end insert_unit;
			

        begin -- read_schematic
			if exists(to_string(name_of_schematic_file)) then
				write_message(
					file_handle => current_output,
					text => "reading schematic file: " & to_string(name_of_schematic_file) & " ...",
					console => true);

				-- log module path as recorded by parent unit
				write_path_to_submodule;
				
				open (file => et_import.schematic_handle, mode => in_file, name => to_string(name_of_schematic_file));
				set_input (et_import.schematic_handle);
				while not end_of_file loop

					-- Store line in variable "line" (see et_string_processing.ads)
					line := et_string_processing.read_line(
								line => get_line,
								number => ada.text_io.line(current_input),
								comment_mark => "", -- there are no comment marks in the schematic file
								ifs => latin_1.space); -- fields are separated by space
					
					case field_count(line) is
						when 0 => null; -- we skip empty lines
						when others =>

--  							put_line("line ->" & to_string(line));
							
							-- read schematic headline like "EESchema Schematic File Version 2"
							if not schematic_headline_processed then
								if get_field_from_line(line,1) = schematic_header_keyword_sys_name and
									get_field_from_line(line,2) = schematic_header_keyword_schematic and
									get_field_from_line(line,3) = schematic_header_keyword_file and
									get_field_from_line(line,4) = schematic_header_keyword_version then
										if positive'value(get_field_from_line(line,5)) = schematic_version then
											-- headline ok, version is supported
											schematic_headline_processed := true;

											-- Save schematic format version in sheet header:                                    
											sheet_header.version := positive'value(get_field_from_line(line,5));
										else
											write_message(
												file_handle => current_output,
												text => message_error & "schematic version" & positive'image(schematic_version) & " required.",
												console => true);                        
											raise constraint_error;
										end if;
								end if;
							else

								-- READ SHEET HEADER: stuff like:
								--     LIBS:nucleo_core-rescue
								--     LIBS:power
								--     LIBS:bel_connectors_and_jumpers
								--     LIBS:bel_primitives
								--     LIBS:bel_stm32
								--     LIBS:nucleo_core-cache
								--     EELAYER 25 0
								--     EELAYER END

								-- This data goes into a the sheet_header. When the schematic file has been
								-- read completely, the sheet_header is appended to global list_of_sheet_headers. 
								-- Why a list of headers ? When schematic files are exported, their headers must be restored to the original state.
								-- NOTE: The library entries in the header are not used by kicad. However, they must be read
								-- and stored in sheet_header.libraries.
								
								-- Field #1 of the line must be broken down by its own ifs in order to get "LIBS" and "bel_stm32"
								if get_field_from_line( get_field_from_line(line,1), 1, latin_1.colon) = schematic_library then

									-- for the log: write library name
									put_line(" uses library " & get_field_from_line( get_field_from_line(line,1), 2, latin_1.colon));

									-- Store bare library name in the list sheet_header.libraries:
									-- We use a doubly linked list because the order of the library names must be kept.
									et_libraries.type_list_of_library_names.append(
										container => sheet_header.libraries,
										new_item => et_libraries.type_library_name.to_bounded_string(
											get_field_from_line( get_field_from_line(line,1), 2, latin_1.colon))
										);

								end if;

								-- layer numbers from a line like "EELAYER 25 0" -- CS: not used ?
								-- CS: we do not read the line "EELAYER END" and assume it is always there.                                                        
								if get_field_from_line(line,1) = schematic_eelayer then
									if get_field_from_line(line,2) = schematic_eelayer_end then
										null;
									else
										-- append layer numbers to the sheet header
										sheet_header.eelayer_a := positive'value(
											get_field_from_line(line,2));

										sheet_header.eelayer_b := natural'value(
											get_field_from_line(line,3));
									end if;
								end if;
							
							
							
								-- READ DESCRIPTION:
								-- If the description reveals there is more than one sheet, we have a hierarchic design. Means we
								-- need to read follwing sheet sections.
								-- The sheet_number_current obtained here serves as part of the coordinates of objects found on this sheet.
								-- The sheet description looks like this:

								-- $Descr A4 11693 8268
								-- encoding utf-8
								-- Sheet 5 8
								-- Title ""
								-- Date ""
								-- Rev ""
								-- Comp ""
								-- Comment1 ""
								-- Comment2 ""
								-- Comment3 ""
								-- Comment4 ""
								-- $EndDescr
								
								if not description_entered then
									if get_field_from_line(line,1) = schematic_description_header then -- $Descr A4 11693 8268
										description_entered := true; -- we are entering the sheet description

										-- read drawing frame dimensions from a line like "$Descr A4 11693 8268"
										drawing_frame_scratch.paper_size	:= type_paper_size'value(get_field_from_line(line,2));
										drawing_frame_scratch.size_x		:= et_libraries.type_grid'value(get_field_from_line(line,3));
										drawing_frame_scratch.size_y 		:= et_libraries.type_grid'value(get_field_from_line(line,4)); 
										drawing_frame_scratch.coordinates.path := path_to_submodule;
										drawing_frame_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));

										-- CS: Other properties of the drawing frame like x/y coordinates, lists of lines and texts are 
										-- kicad built-in things and remain unassigned here.
																	
									end if;
								else -- we are inside the description
									if get_field_from_line(line,1) = schematic_description_footer then -- $EndDescr
										description_entered := false; -- we are leaving the description
										description_processed := true;

										-- Make temporarily title_block_scratch complete by assigning coordinates and list of texts.
										-- Then purge temporarily list of texts.
										-- Then append temporarily title block to main module.
										title_block_scratch.coordinates.path := path_to_submodule;
										title_block_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
										title_block_scratch.texts := list_of_title_block_texts_scratch; -- assign collected texts list to temporarily title block
										-- CS: x/y coordinates and list of lines are kicad built-in things and thus not available currently.

										-- purge temporarily texts
										type_list_of_title_block_texts.delete(list_of_title_block_texts_scratch,1,
											type_list_of_title_block_texts.length(list_of_title_block_texts_scratch));

										-- append title block to main module
										type_list_of_title_blocks.append(module.title_blocks,title_block_scratch);
										
										-- append temporarily drawing frame to main module
										type_list_of_frames.append(module.frames,drawing_frame_scratch);
									end if;

									-- read endcoding from a line like "encoding utf-8"
									-- CS: checks only for a non-default endcoding and outputs a warning.
									-- CS: we assume only one encoding. other encodings are ignored currently.
									-- The encoding should be project wide. KiCad allows a sheet specific encoding which is no
									-- good idea.
									if get_field_from_line(line,1) = schematic_keyword_encoding then
										if get_field_from_line(line,2) /= encoding_default then
											put_line(message_warning & "non-default endcoding '" & 
												get_field_from_line(line,2) & "' found !");
										end if;
									end if;
										
									-- read sheet number from a line like "Sheet 1 7"
									if get_field_from_line(line,1) = schematic_keyword_sheet then
										sheet_number_current := positive'value(get_field_from_line(line,2));
										put_line(" sheet" & positive'image(sheet_number_current) & " ...");
										sheet_count_total    := positive'value(get_field_from_line(line,3));
										if sheet_count_total > 1 then
											-- Set in the list_of_submodules (to be returned) the parent_module. The schematic file 
											-- being processed (see input parameters of read_file_schematic_kicad) becomes the parent module
											-- of the submodules here.
											list_of_submodules.parent_module := type_submodule_name.to_bounded_string(to_string(name_of_schematic_file));
										end if;
										-- CS: make sure total sheet count is less or equal current sheet number.

										-- Our temporarily drawing frame gets the current sheet number assigned.
										drawing_frame_scratch.coordinates.sheet_number := sheet_number_current;
									end if;						

									-- read sheet title from a line like "Title "abc""
									if get_field_from_line(line,1) = schematic_keyword_title then                        
										title_block_text_scratch.meaning := TITLE;
										title_block_text_scratch.text := type_title_block_text_string.to_bounded_string(
											strip_quotes((get_field_from_line(line,2))));
										type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
									end if;

									-- read date from a line like "Date "1981-01-23""
									if get_field_from_line(line,1) = schematic_keyword_date then                        
										title_block_text_scratch.meaning := DRAWN_DATE;
										title_block_text_scratch.text := type_title_block_text_string.to_bounded_string(
											strip_quotes((get_field_from_line(line,2))));
										type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
									end if;

									-- read revision from a line like "Rev "9.7.1"
									if get_field_from_line(line,1) = schematic_keyword_revision then                        
										title_block_text_scratch.meaning := REVISION;
										title_block_text_scratch.text := type_title_block_text_string.to_bounded_string(
											strip_quotes((get_field_from_line(line,2))));
										type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
									end if;

									-- read company name
									if get_field_from_line(line,1) = schematic_keyword_company then
										title_block_text_scratch.meaning := COMPANY;
										title_block_text_scratch.text := type_title_block_text_string.to_bounded_string(
											strip_quotes((get_field_from_line(line,2))));
										type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
									end if;

									-- read commments 1..4 CS: need something more flexible here in order to read any number of comments.
									if  get_field_from_line(line,1) = schematic_keyword_comment_1 or
										get_field_from_line(line,1) = schematic_keyword_comment_2 or
										get_field_from_line(line,1) = schematic_keyword_comment_3 or 
										get_field_from_line(line,1) = schematic_keyword_comment_4 then
											title_block_text_scratch.meaning := MISC;
											title_block_text_scratch.text := type_title_block_text_string.to_bounded_string(
												strip_quotes((get_field_from_line(line,2))));
											type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
									end if;
									

								end if;

								-- Read submodule (sheet) sections (if there has been a total sheet count greater 1 detected earlier).
								-- NOTE: Such sections solely serve to display a hierarchical sheet as a black box with its ports.
								-- Rightly said this is the black box representation of a submodule. 
								-- So in the following we refer to them as "submodule".
								-- A submodule (sheet) section example:
								
								-- $Sheet
								-- S 4050 5750 1050 650 
								-- U 58A73B5D
								-- F0 "Sheet58A73B5C" 58
								-- F1 "morpho_test.sch" 58
								-- $EndSheet

								-- And add name of submodule (sheet file name) to list_of_submodules:
								if sheet_count_total > 1 then
									if not sheet_description_entered then
										if get_field_from_line(line,1) = schematic_sheet_header then -- $Sheet
											sheet_description_entered := true;
										end if;
									else -- we are inside a sheet description
										if get_field_from_line(line,1) = schematic_sheet_footer then -- $EndSheet
											sheet_description_entered := false; -- we are leaving the sheet description

											-- append name_of_submodule_scratch to list_of_submodules to be returned to parent unit
											type_list_of_submodule_names.append(list_of_submodules.list, name_of_submodule_scratch);

											-- append submodule_gui_scratch to list of gui submodules
											type_list_of_gui_submodules.append(module.submodules, submodule_gui_scratch);
										end if;

										-- read GUI submodule (sheet) position and size from a line like "S 4050 5750 1050 650"
										if get_field_from_line(line,1) = schematic_keyword_sheet_pos_and_size then
											submodule_gui_scratch.coordinates.path := path_to_submodule;
											submodule_gui_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
											submodule_gui_scratch.coordinates.sheet_number := sheet_number_current;
											submodule_gui_scratch.coordinates.x := et_libraries.type_grid'value(get_field_from_line(line,2));
											submodule_gui_scratch.coordinates.y := et_libraries.type_grid'value(get_field_from_line(line,3));
											submodule_gui_scratch.size_x		:= et_libraries.type_grid'value(get_field_from_line(line,4));
											submodule_gui_scratch.size_y		:= et_libraries.type_grid'value(get_field_from_line(line,5));                                
										end if;

										-- read GUI submodule (sheet) timestamp from a line like "U 58A73B5D"
										if get_field_from_line(line,1) = schematic_keyword_sheet_timestamp then 
											submodule_gui_scratch.timestamp := get_field_from_line(line,2);
										end if;
										
										-- Read submodule (sheet) name from a line like "F0 "mcu_stm32f030" 60"
										-- Since this is the black-box-representation of a kicad-sheet its name is threated as name of a submodule.
										-- The sheet name is stored in submodule_gui_scratch.name to be compared with the sheet file name later.
										if get_field_from_line(line,1) = schematic_keyword_sheet_name then
											submodule_gui_scratch.name := type_submodule_name.to_bounded_string(strip_quotes(get_field_from_line(line,2)));
											submodule_gui_scratch.text_size_of_name := et_libraries.type_text_size'value(get_field_from_line(line,3));
										end if;

										-- Read sheet file name from a line like "F1 "mcu_stm32f030.sch" 60".
										-- The file name (name_of_submodule_scratch) goes into the list of submodules to be returned to the parent unit.
										if get_field_from_line(line,1) = schematic_keyword_sheet_file then
											name_of_submodule_scratch := type_submodule_name.to_bounded_string(strip_quotes(get_field_from_line(line,2)));
											submodule_gui_scratch.text_size_of_file := et_libraries.type_text_size'value(get_field_from_line(line,3));
											
											-- Test if sheet name and file name match:
											if type_submodule_name.to_string(submodule_gui_scratch.name) /= base_name(type_submodule_name.to_string(name_of_submodule_scratch)) then
												put_line(message_warning & "name mismatch: sheet: " &
													type_submodule_name.to_string(submodule_gui_scratch.name) &
													" file: " & type_submodule_name.to_string(name_of_submodule_scratch));
											end if;
										end if;

									end if;
								end if;

								-- Further parts of the file can be read IF the description has been processed before (see above)
								if description_processed then
									
									-- read net segments						
									if not net_segment_entered then
										-- collect net segments
										if get_field_from_line(line,1) = schematic_keyword_wire then
											if get_field_from_line(line,2) = schematic_keyword_wire then
												if get_field_from_line(line,3) = schematic_keyword_line then
													--put_line(to_string(line_of_schematic_file));
													net_segment_entered := true; -- CS: assumption: segment coordinates follow in next line
												end if;
											end if;
										end if;
									else
										net_segment_entered := false; -- we are leaving a net segment
										--put_line(to_string(line_of_schematic_file));

										-- CS: warning on segment with zero length
										
										-- Build a temporarily net segment with fully specified coordinates:
										segment_scratch.coordinates_start.path := path_to_submodule;
										
										-- the name of the current submodule, which is in case of kicad the subordinated schematic file
										segment_scratch.coordinates_start.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
										segment_scratch.coordinates_end.module_name   := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
										
										-- The sheet number. NOTE: Kicad V4 can handle only one sheet per submodule. The sheet numbering is consecutive and does
										-- not care about the actual submodule names.
										segment_scratch.coordinates_start.sheet_number := sheet_number_current;

										-- the x/y position
										segment_scratch.coordinates_start.x := et_libraries.type_grid'value(get_field_from_line(line,1));
										segment_scratch.coordinates_start.y := et_libraries.type_grid'value(get_field_from_line(line,2));
										segment_scratch.coordinates_end.x   := et_libraries.type_grid'value(get_field_from_line(line,3));
										segment_scratch.coordinates_end.y   := et_libraries.type_grid'value(get_field_from_line(line,4));

										-- The net segments are to be collected in a wild list of segments for later sorting. 
										type_wild_list_of_net_segments.append(wild_segment_collection,segment_scratch);
									end if;

									-- read net junctions and store them in a wild list of net junctions for later sorting
									if get_field_from_line(line,1) = schematic_keyword_connection then
										if get_field_from_line(line,2) = schematic_tilde then

											-- build a temporarily junction
											junction_scratch.coordinates.path := path_to_submodule;
											junction_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
											junction_scratch.coordinates.sheet_number := sheet_number_current;
											junction_scratch.coordinates.x := et_libraries.type_grid'value(get_field_from_line(line,3));
											junction_scratch.coordinates.y := et_libraries.type_grid'value(get_field_from_line(line,4));
											type_list_of_net_junctions.append(wild_collection_of_junctions,junction_scratch);
											junction_count := junction_count + 1;
										end if;
									end if;
										
									-- Read simple net labels (they do not have a tag, but just a text) 
									-- CS: assumption: keywords "Text Label" and coordinates in one line
									if not simple_label_entered then							
										if 	get_field_from_line(line,1) = schematic_keyword_text and 
											get_field_from_line(line,2) = schematic_keyword_label_simple then

											simple_label_entered := true;
											--put_line(to_string(line_of_schematic_file));

											-- Build a temporarily simple label from a line like "Text Label 5350 3050 0    60   ~ 0" :
											label_simple_scratch.coordinates.path := path_to_submodule;
											label_simple_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
											label_simple_scratch.coordinates.sheet_number := sheet_number_current;
											label_simple_scratch.coordinates.x := et_libraries.type_grid'value(get_field_from_line(line,3));
											label_simple_scratch.coordinates.y := et_libraries.type_grid'value(get_field_from_line(line,4));
											label_simple_scratch.orientation   := to_orientation(get_field_from_line(line,5));

											-- build text attributes from size, font and line width
-- 											label_simple_scratch.text_attributes := to_text_attributes(
-- 												size  => type_text_size'value(get_field_from_line(line,6)),
-- 												style => get_field_from_line(line,7),
-- 												width => type_text_line_width'value(get_field_from_line(line,8)));

											label_simple_scratch.size := et_libraries.type_text_size'value (get_field_from_line(line,6));
											label_simple_scratch.style := to_text_style (style_in => get_field_from_line(line,7), text => true);
											label_simple_scratch.width := et_libraries.type_text_line_width'value(get_field_from_line(line,8));
											
											
										end if;
									else
										simple_label_entered := false; -- we are leaving a simple label

										-- get label text and put it to temporarily simple label
										label_simple_scratch.text := type_net_name.to_bounded_string(get_field_from_line(line,1));

										-- for the log
										write_label_properties (
											label => type_net_label(label_simple_scratch),
											indentation => 2);

										-- The simple labels are to be collected in a wild list of simple labels.
										type_list_of_labels_simple.append (wild_simple_label_collection_scratch,label_simple_scratch);
									end if;
									
									-- read tag net labels (tagged labels can be global or hierarchical)
									if not tag_label_entered then
										if 	get_field_from_line(line,1) = schematic_keyword_text 
											and 
											(get_field_from_line(line,2) = schematic_keyword_label_hierarchic 
											or get_field_from_line(line,2) = schematic_keyword_label_global)
											then
										
											tag_label_entered := true;
											--put_line(to_string(line_of_schematic_file));

											-- Build a temporarily hierarchic/global label from a line like "Text GLabel 1850 3100 0 58 BiDi ~ 0"
											-- The keyword in field 2 tells whether we have a hierarchic or global label:
											if get_field_from_line(line,2) = schematic_keyword_label_hierarchic then
												label_tag_scratch.hierarchic := true;
												label_tag_scratch.global := false;
											else
												label_tag_scratch.hierarchic := false;
												label_tag_scratch.global := true;
											end if;

											label_tag_scratch.coordinates.path := path_to_submodule;
											label_tag_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
											label_tag_scratch.coordinates.sheet_number := sheet_number_current;
											label_tag_scratch.coordinates.x := et_libraries.type_grid'value(get_field_from_line(line,3));
											label_tag_scratch.coordinates.y := et_libraries.type_grid'value(get_field_from_line(line,4));
											label_tag_scratch.orientation   := to_orientation(get_field_from_line(line,5));
											
											label_tag_scratch.direction := to_direction(
												get_field_from_line(line,7)
												);

											-- build text attributes from size, font and line width
											label_tag_scratch.size := et_libraries.type_text_size'value(get_field_from_line(line,6));
											label_tag_scratch.style := to_text_style (style_in => get_field_from_line(line,8), text => true);
											label_tag_scratch.width := et_libraries.type_text_line_width'value(get_field_from_line(line,9));
										end if;
									else
										tag_label_entered := false; -- we are leaving a tag label

										-- get label text and put it to temporarily tag label
										label_tag_scratch.text := type_net_name.to_bounded_string(get_field_from_line(line,1));

										-- for the log
										write_label_properties (
											label => type_net_label(label_tag_scratch),
											indentation => 2);
										
										-- The tag labels are to be collected in a wild list of tag labels for later sorting.
										type_list_of_labels_tag.append(wild_tag_label_collection_scratch,label_tag_scratch);
									end if;

									-- read note from a line like "Text Notes 3400 2800 0 60 Italic 12" followed by a line with the actual note:
									if not note_entered then
										if 	get_field_from_line(line,1) = schematic_keyword_text and 
											get_field_from_line(line,2) = schematic_keyword_note then
												note_entered := true; -- we are entering a note
										
												-- set coordinates
												note_scratch.coordinates.path := path_to_submodule;
												note_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
												note_scratch.coordinates.sheet_number := sheet_number_current;
												note_scratch.coordinates.x := et_libraries.type_grid'value(get_field_from_line(line,3));
												note_scratch.coordinates.y := et_libraries.type_grid'value(get_field_from_line(line,4));
												note_scratch.orientation   := to_orientation(get_field_from_line(line,5));

-- 												note_scratch.attributes := to_text_attributes(
-- 													size  => type_text_size'value(get_field_from_line(line,6)),
-- 													style => get_field_from_line(line,7),
-- 													width => type_text_line_width'value(get_field_from_line(line,8)));
												note_scratch.size := et_libraries.type_text_size'value(get_field_from_line(line,6));
												note_scratch.style := to_text_style (style_in => get_field_from_line(line,7), text => true);
												note_scratch.line_width := et_libraries.type_text_line_width'value(get_field_from_line(line,8));

										end if;
									else 
										note_entered := false; -- we are leaving a note

										-- get note text from a line like "hello\ntest". NOTE "\n" represents a line break
										-- CS: store lines in a list of lines instead ?
										-- CS: Currently we store the line as it is in note_scratch.text
										note_scratch.content := et_libraries.type_text_content.to_bounded_string(to_string(line));

										write_note_properties(note_scratch,2); -- last actual is indentation
										
										-- the notes are to be collected in the list of notes
										et_schematic.type_texts.append (module.notes,note_scratch);
									end if;
									
									-- READ COMPONENTS
									-- Once a component header ($Comp) found, set component_entered flag. This indicates we are inside a component section.
									-- Inside the component section, we process its content until the component footer ($EndComp) is found.
									-- Some entries of the component section are relevant for the whole component. Some entries are unit specific.
									-- The component section looks like this example:
									
									-- $Comp
									-- L 74LS00 U1		-- component specific
									-- U 4 1 5965E676	-- unit specific
									-- P 4100 4000		-- unit specific
									-- F 0 "U1" H 4100 4050 50  0000 C CNN		-- text fields
									-- F 1 "74LS00" H 4100 3900 50  0000 C CNN	
									-- F 2 "bel_ic:S_SO14" H 4100 4000 50  0001 C CNN
									-- F 3 "" H 4100 4000 50  0001 C CNN
									-- 	4    4100 4000		-- CS: unknown
									-- 	1    0    0    -1 	-- CS: unknown
									-- $EndComp
									
									if not component_entered then
										if get_field_from_line(line,1) = schematic_component_header then
											component_entered := true;

											-- This is to init the temporarily used variables that store text fields.
											-- This clears the "field found" flags
											init_temp_variables;

										end if;
									else -- we are inside the component and wait for the component footer ($EndComp)
										if get_field_from_line(line,1) = schematic_component_footer then
											component_entered := false; -- we are leaving the component

											-- Check if all required text fields have been found.
											-- Check content of text fields for syntax and plausibility.
											check_text_fields;
											
											-- Insert component in component list of module. If a component is split
											-- in units, only the first occurence of it leads to inserting the component.
											-- Nevertheless there are some checks on the unit (see insert_component).
											insert_component;
	
											-- The component_cursor now points to the component in the component list.
											-- We update the component with the collected unit information.
											type_components.update_element (module.components, component_cursor, insert_unit'access);

										else
											--put_line("line ->" & to_string(line));
											-- READ COMPONENT SECTION CONTENT
											
											-- Read component name and annotation from a line like "L NetChanger N1". 
											-- From this entry we reason the compoenent appearance.
											if get_field_from_line(line,1) = schematic_component_identifier_name then -- "L"
												tmp_component_name_in_lib := et_libraries.type_component_name.to_bounded_string(get_field_from_line(line,2)); -- "SN74LS00"
												tmp_component_appearance := to_appearance(line => line, schematic => true);
												case tmp_component_appearance is
												
													when sch => 
														-- we have a line like "L P3V3 #PWR07"
														tmp_component_reference := et_general.to_component_reference(
																text_in => get_field_from_line(line,3),
																allow_special_character_in_prefix => true);

													when sch_pcb =>

														-- we have a line like "L 74LS00 U1"
														tmp_component_reference := et_general.to_component_reference(
																text_in => get_field_from_line(line,3),
																allow_special_character_in_prefix => false);

													when others => -- CS: This should never happen. A subtype of type_component_appearance could be a solution.
														null;
														raise constraint_error;
														
												end case;
															
												-- CS: check proper annotation
											end if;

											-- read line like "U 2 1 4543D4D3F" 
											-- U is the line indicator, 2 is the unit id, 1 is the demorgan flag, last field is the timestamp
											if get_field_from_line(line,1) = schematic_component_identifier_unit then -- "U"

												-- KiCad uses positive numbers to identifiy units. But in general a unit name can
												-- be a string as well. Therefore we handle the unit id as string.
												tmp_component_unit_name := et_libraries.type_unit_name.to_bounded_string(
													get_field_from_line(line,2)); -- the unit id

												-- Read DeMorgan flag:
												tmp_component_alt_repres := to_alternative_representation(line => line, schematic => true);

												-- Read timestamp:
												tmp_component_timestamp := type_timestamp(get_field_from_line(line,4)); 
												-- CS: function that converts to a type_timestamp and does a plausibility check
											end if;

											-- Read unit coordinates from a line like "P 3200 4500".
											if get_field_from_line(line,1) = schematic_component_identifier_coord then -- "P"
												tmp_component_position.x := et_libraries.type_grid'value(
													get_field_from_line(line,2)); -- "3200"
												tmp_component_position.y := et_libraries.type_grid'value(
													get_field_from_line(line,3)); -- "4500"

												-- The unit coordinates is more than just x/y :
												-- unit_scratch.coordinates.main_module := module.name;
												tmp_component_position.path := path_to_submodule;
												tmp_component_position.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
												tmp_component_position.sheet_number := sheet_number_current;
											end if;

											-- read unit fields 0..2 from lines like:
											-- 			"F 0 "N701" H 2600 2100 39  0000 C CNN"
											--			"F 1 "NetChanger" H 2600 2250 60  0001 C CNN"
											--			"F 2 "bel_netchanger:N_0.2MM" H 2600 2100 60  0001 C CNN"
											--
											-- set "field found" flags
											if get_field_from_line(line,1) = component_field_identifier then -- "F"
												
												case type_component_field_id'value(get_field_from_line(line,2)) is
													when component_field_reference =>
														tmp_component_text_reference_found	:= true;
														tmp_component_text_reference 		:= to_text;

													when component_field_value =>
														tmp_component_text_value_found		:= true;
														tmp_component_text_value 			:= to_text;
														
													when component_field_footprint =>
														tmp_component_text_packge_found		:= true;
														tmp_component_text_packge 			:= to_text;
														
													when component_field_datasheet =>
														tmp_component_text_datasheet_found	:= true;
														tmp_component_text_datasheet 		:= to_text;
														
													when component_field_function =>
														tmp_component_text_fnction_found	:= true;
														tmp_component_text_fnction 			:= to_text;
														
													when component_field_partcode =>
														tmp_component_text_partcode_found	:= true;
														tmp_component_text_partcode 		:= to_text;
														
													when component_field_commissioned =>
														tmp_component_text_commissioned_found	:= true;
														tmp_component_text_commissioned 		:= to_text;
														
													when component_field_updated =>
														tmp_component_text_updated_found	:= true;
														tmp_component_text_updated 			:= to_text;
														
													when component_field_author =>
														tmp_component_text_author_found		:= true;
														tmp_component_text_author 			:= to_text;

													when others => null; -- CS: other fields are ignored
												end case;

											end if;
									end if;
									
								end if;
							end if;

							end if; -- if not schematic_header_processed

					end case;

				end loop;

                -- If file has been read and no header found:
                if not schematic_headline_processed then
                    write_message(
						file_handle => current_output,
						text => message_error & "Schematic file header invalid or not found ! File not accepted !",
						console => true);
                    raise constraint_error;
                end if;

                -- Add sheet_header to global list_of_sheet_headers.
                -- NOTE: The file name serves as key in order to match from file to header.
                type_list_of_sheet_headers.insert(
                    container => list_of_sheet_headers, 
                    key => type_sheet_file.to_bounded_string(to_string(name_of_schematic_file)),
                    new_item => sheet_header);
                
				close ( et_import.schematic_handle );
				
				-- Build anonymous nets:
				-- We are processing the net segments of a sheet here. The net segments have been collected in a wild list of net segments earlier.
				-- This list does not reveal the actual nets where the segments belong to. The segments are inspected in the following by
				-- looking at the coordinates of their start and end point. Segments whose start or end points match other segments are considered
				-- as connected to each other (means they belong to the same net). The net name is unknown yet. So the outcome of the following is
				-- a list of anonymous nets.
				-- CS: handle circlular nets, currently they cause a forever-loop here
				segment_count := natural(type_wild_list_of_net_segments.length(wild_segment_collection)); -- get number of segments on the current sheet
				put_line("  processing" & natural'image(segment_count) & " net segments ...");

				-- It may happen that a sheet has no nets, for example the top level sheet of a design with global nets only. If there are no net segments
				-- at all, skip processing net segments.
				if segment_count > 0 then 

					-- Segments where a junction sits on, must be broken down. This results in more segments than calculated earlier.
					-- The outcome of process_junctions might be a greater number of net segments than currently being held in segment_count.
					process_junctions;
					-- segment_count now has been updated
					
					-- We inspect one segment after an other. Variable seg points to the segment being processed. 
					-- A segment, whose e AND s flag has been set, is to be skipped (because this segment has been processed already).
					-- Variable side_scratch points to the side of the segment (start or end point) where another matching segment is to be searched for.
					-- If a matching segment is found, it gets appended to the current anonymous net.
					for i in 1..segment_count loop
						seg := i; 
						if not type_wild_list_of_net_segments.element(wild_segment_collection,seg).s and -- Skip already processed nets.
						   not type_wild_list_of_net_segments.element(wild_segment_collection,seg).e then 

						    -- We initiate a new anonymous net and start looking for a matching segment on the end_point:
							--put_line(et_import.report_handle," anonymous net" & positive'image(seg) & ":"); 
							put_line("  anonymous net with segments:");
											
							add_segment_to_anonymous_net(seg); 
							side_scratch := end_point;

							loop -- A
								--put_line(et_import.report_handle,"  --> A");
								-- Search for a segment connected to the current segment. 
								-- If function search_for_same_coordinates discovers a suitable segment, it adds the segment to current anonymous net.
								-- search_for_same_coordinates sets the e or s flag of the segment in order to indicate which end point has been processed.
								-- If no connected segment found, toggle side_scratch and repeat search_for_same_coordinates on the other side of the segment.
								same_coord_result := search_for_same_coordinates( id => seg, seg_in => type_wild_list_of_net_segments.element(wild_segment_collection,seg),
																				  side => side_scratch);
								if same_coord_result.valid then
									--put_line(et_import.report_handle,"  --> E");
									null;
								else
									-- Toggle side_scratch depending on the e/s flag of the segment:
									-- D
 									if type_wild_list_of_net_segments.element(wild_segment_collection,seg).e then
										-- put_line(et_import.report_handle,"  --> D1");
										side_scratch := start_point;
									end if;
									
 									if type_wild_list_of_net_segments.element(wild_segment_collection,seg).s then
										-- put_line(et_import.report_handle,"  --> D2");	
 										side_scratch := end_point;	
 									end if;

									-- C
									--put_line(et_import.report_handle,"  --> C");
									-- Search for a segment connected to the current segment (starting from the current side_scratch).
									-- If function search_for_same_coordinates discovers a suitable segment, it adds the segment to current anonymous net.
									-- search_for_same_coordinates sets the e or s flag of the segment in order to indicate which end point has been processed.
									-- If no connected segment found, the current anonymous net is considered as complete -> cancel loop, advance to next segment ...
									same_coord_result := search_for_same_coordinates( id => seg, seg_in => type_wild_list_of_net_segments.element(wild_segment_collection,seg),
																					  side => side_scratch);
									if same_coord_result.valid then
										--put_line(et_import.report_handle,"  --> F");	
										null;
									else
										--put_line(et_import.report_handle,"  done");
										add_net_to_list_of_anonymous_nets; 	-- All collected segments belong to the same net. This net is to be added to
																			-- a list of anonymous nets.
										exit;	-- no further segment search required.
									end if;
								end if;

								-- B
								--put_line(et_import.report_handle,"  --> B");
								-- Update seg with the id of the segment found by search_for_same_coordinates. So seg now points to the next connected segment. 
								-- same_coord_result contains the end point of the net that has been found.
								-- Depending on the end point of the matching net, side_scratch must be set so that the search can continue on the other side
								-- of the new segment.
								seg := same_coord_result.id;
								case same_coord_result.side is
									when end_point => 
										side_scratch := start_point;
									when start_point =>
										side_scratch := end_point;
								end case;
							end loop;
						end if;
					end loop;
				end if;


				-- All anonymous nets must be given a name. The name is enforced by the a net label. The first label found on the net sets the net name.
				-- Other labels on the net are checke for their name only. If the name differs from the net name set earlier, a warning is output.
				-- Nets without label remain anonymous by using the notation "N$"
				associate_net_labels_with_anonymous_nets;

				-- CS: add_pins_to_nets
				
			else
				--put_line(message_error & "Schematic file '" & to_string(name_of_schematic_file) & "' not found !");
				write_message(
					file_handle => current_output,
					text => message_error & "schematic file '" & to_string(name_of_schematic_file) & "' not found !",
					console => true);
				raise constraint_error;
			end if;

			return list_of_submodules;
			
			exception
				when event:
					constraint_error =>
						put_line(exception_information(event));
						put_line(message_error & "in schematic file '" & to_string(name_of_schematic_file) & "' " & et_string_processing.affected_line(line));
						raise;
						return list_of_submodules;
				when others =>
						put_line(message_error & "in schematic file '" & to_string(name_of_schematic_file) & "' " & et_string_processing.affected_line(line));
						raise;					
						return list_of_submodules;

		end read_schematic;


		list_of_submodules : type_list_of_submodule_names_extended;
		top_level_schematic_file, name_of_schematic_file : et_import.type_schematic_file_name.bounded_string;

		package stack_of_sheet_lists is new stack_lifo(max => 10, item => type_list_of_submodule_names_extended);
        use stack_of_sheet_lists;
        
    begin -- import design
		create_report; -- directs all puts to the report file
		
		case et_import.cad_format is
			when kicad_v4 =>

				-- derive top level schematic file name from project file (they differ only in their extension)
				top_level_schematic_file := read_project_file;
				read_components_libraries; -- as stored in lib_dir and project_libraries
				name_of_schematic_file := top_level_schematic_file;

                -- The top level schematic file dictates the module name. At the same time it is the first entry
                -- in the module path.
				module.name := type_submodule_name.to_bounded_string(to_string(name_of_schematic_file));
                append_name_of_parent_module_to_path(module.name);
                
				-- Starting from the top level module, we read its schematic file. The result can be a list of submodules.
				-- NOTE: Kicad refers to them as "sheets" !
				
				-- The function read_schematic requires the name of the current submodule,
				-- It returns a list of submodules.
				list_of_submodules := read_schematic(name_of_schematic_file => name_of_schematic_file);
				
				put("  DESIGN STRUCTURE ");

				-- If read_file_schematic_kicad returns an empty list of submodules, we are dealing with a flat design. Otherwise
				-- the design is hierarchic (because the submodule list is longer than zero).
				if type_list_of_submodule_names.length(list_of_submodules.list) = 0 then -- flat design
					put_line("FLAT");
				else -- hierarchic design
					-- In the follwing we dive into the submodules. Each time before a deeper level is entered,
					-- the list of submodules of the current level is saved on a LIFO stack.
					-- The top level schematic is at level 0. The level decreases (negative) each time a deeper
					-- level is entered.
					put_line("HIERARCHIC");
					
					stack_of_sheet_lists.init; -- stack init

					-- output the number of submodules (sheets) found at level 0:
					put_line("  number of hierarchic sheets" & natural'image(
						natural(type_list_of_submodule_names.length(list_of_submodules.list))));

					-- Initially set submodule pointer at first submodule of list:
					list_of_submodules.id := 1;
                    
					loop
						-- fetch name of submodule (where id is pointing at)
						name_of_schematic_file := to_bounded_string(type_submodule_name.to_string(
							type_list_of_submodule_names.element(container => list_of_submodules.list,index => list_of_submodules.id)));
						
						-- backup list_of_submodules OF THIS LEVEL on stack (including the current submodule id)
						push(list_of_submodules);
						put_line("DESCENDING TO HIERARCHY LEVEL -" & trim(natural'image(depth),left));
						put_line(row_separator_single);
						
						-- Read schematic file as indicated by list_of_submodules.id. 
						-- Read_schematic receives the name of the schematic file to be read.
						list_of_submodules := read_schematic(name_of_schematic_file => name_of_schematic_file);

						-- If the schematic file contains submodules (hierarchic sheets), set list_of_submodules.id to the first 
						-- submodule of them. Otherwise restore submodule list of parent module and advance therein to next submodule.
						if type_list_of_submodule_names.length(list_of_submodules.list) = 0 then -- flat submodule (no hierarchic sheets)

							list_of_submodules := pop;
                            list_of_submodules.id := list_of_submodules.id + 1;
                            --delete_last_module_name_from_path;
							put_line("NO SUBMODULES HERE. ASCENDING TO HIERARCHY LEVEL -" & trim(natural'image(depth),left));
							put_line(row_separator_single);

						else
							-- set cursor at first submodule of list and append name of parent module to path_to_submodule
                            list_of_submodules.id := 1;
                            append_name_of_parent_module_to_path(list_of_submodules.parent_module);
						end if;

						-- Once the last submodule of the list has been processed, restore list of the overlying level and advance to next module.
						-- Exit after last submodule in level 0 has been processed.
						if list_of_submodules.id > positive(type_list_of_submodule_names.length(list_of_submodules.list)) then
							if depth = 0 then 
								put_line("LAST SUBMODULE PROCESSED.");
								exit; 
							end if;
							list_of_submodules := pop; -- restore overlying list
                            list_of_submodules.id := list_of_submodules.id + 1;
                            delete_last_module_name_from_path; -- update path_to_submodule
							put_line("LAST SUBMODULE PROCESSED. ASCENDING TO HIERARCHY LEVEL: -" & trim(natural'image(depth),left));
							put_line(row_separator_single);
						end if;
						
					end loop;
				end if;
			when others =>
				null;
		end case;

		close_report;

		-- CS: exception handler
		
	end import_design;

end et_kicad;

-- Soli Deo Gloria

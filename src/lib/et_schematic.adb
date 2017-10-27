------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET SCHEMATIC DECLARATIONS                      --
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
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with ada.directories;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;
with et_coordinates;
with et_string_processing;
with et_geometry;
with et_export;
with et_csv;
with et_netlist;

package body et_schematic is

	function to_string (schematic : in type_schematic_file_name.bounded_string) return string is
	-- Returns the given schematic file name as string.
	begin
		return type_schematic_file_name.to_string (schematic);
	end to_string;
	
	-- Sometimes we need to output the location of a submodule:
	procedure write_path_to_submodule is
		use et_coordinates;
		c : type_path_to_submodule.cursor;
		use et_string_processing;
	begin
		log (text => "path/location");
		log_indentation_up;
		
		c := type_path_to_submodule.first (path_to_submodule);            

		-- If there is a hierarchy deeper than 1, write path to submodule:
		if type_path_to_submodule.length(path_to_submodule) > 1 then
			for n in 1..type_path_to_submodule.length(path_to_submodule)-1 loop
				log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
				c := type_path_to_submodule.next(c);
			end loop;
		
			c := type_path_to_submodule.last(path_to_submodule);

			-- write the submodule name
			log_indentation_up;
			log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
			log_indentation_down;
		else
			-- no hierarchy. write just the submodule name
			log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
		end if;
		
		log_indentation_down;
	end write_path_to_submodule;

	-- Here we append a submodule name the the path_to_submodule.
	procedure append_name_of_parent_module_to_path (submodule : in et_coordinates.type_submodule_name.bounded_string) is
		use et_string_processing;
		use ada.directories;
		use et_coordinates;
	begin
		log (text => "path_to_submodule: appending submodule " & type_submodule_name.to_string(submodule), level => 2);
		-- Since we are dealing with file names, the extension must be removed before appending.
		type_path_to_submodule.append (path_to_submodule,
			type_submodule_name.to_bounded_string (base_name (type_submodule_name.to_string(submodule)))
			);
	end append_name_of_parent_module_to_path;
	
	-- Here we remove the last submodule name form the path_to_submodule.
	procedure delete_last_module_name_from_path is
		use et_coordinates;
	begin
		type_path_to_submodule.delete_last (path_to_submodule);
	end delete_last_module_name_from_path;

	function to_string (net_name : in type_net_name.bounded_string) return string is
	-- Returns the given net name as string.
	begin
		return type_net_name.to_string (net_name);
	end to_string;

	function anonymous (net_name : in type_net_name.bounded_string) return boolean is
	-- Returns true if the given net name is anonymous.
	begin
		if slice (net_name, 1, 2) = anonymous_net_name_prefix then
			return true;
		else 
			return false;
		end if;
	end anonymous;
	
	procedure write_label_properties (label : in type_net_label) is
	-- Writes the properties of the given net label in the logfile.
		use et_string_processing;
		use et_coordinates;

		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		case label.label_appearance is
			when simple =>
				log (text => "simple label");
			when tag =>
				log (text => "tag label");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_up;
		log ("name '" & type_net_name.to_string (label.text) & "' ");
		log (to_string (label.coordinates), log_threshold);
		log (to_string (label.orientation), log_threshold);
		
		case label.label_appearance is
			when simple =>
				null;
			when tag =>
				null;
				--put("tag label ");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_down;
		log_indentation_down;

	end write_label_properties;

	procedure write_junction_properties (junction : in type_net_junction) is
	-- Writes the properties of the given net junction in the logfile.
		use et_string_processing;
		use et_coordinates;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		log ("net junction");
		log_indentation_up;
		log (to_string (junction.coordinates), log_threshold);
		log_indentation_down;
		log_indentation_down;		
	end write_junction_properties;
	
	procedure write_note_properties (note : in et_schematic.type_note) is
	-- Writes the properties of the given note
		use et_string_processing;
		use et_coordinates;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		log ("text note");

		log_indentation_up;

		-- content
		if et_libraries.type_text_content.length (note.content) > 0 then
			log ("content '" & type_text_content.to_string (note.content) & "'");
		else
			log (et_string_processing.message_warning & "no content !"); 
		end if;

		
		if log_level >= log_threshold then
			
			-- position
			log (to_string (note.coordinates), log_threshold);
			
			-- size
			log ("size" & et_libraries.type_text_size'image (note.size));

			-- style
			log ("style " & to_lower(et_libraries.type_text_style'image (note.style)));

			-- line width
			log ("line width" & et_libraries.type_text_line_width'image (note.line_width));

			-- angle
			log (to_string (note.orientation));

			-- visible
			log ("visible " & to_lower (et_libraries.type_text_visible'image (note.visible)));

			-- alignment
			log ("alignment (hor/vert) "
				& to_lower (et_libraries.type_text_alignment_horizontal'image (note.alignment.horizontal))
				& "/"
				& to_lower (et_libraries.type_text_alignment_vertical'image (note.alignment.vertical)));

		end if;
		
		log_indentation_down;
		log_indentation_down;
	end write_note_properties;
	
	procedure write_component_properties ( component : in type_components.cursor) is
	-- Writes the properties of the component indicated by the given cursor.
		use et_string_processing;
	begin
		log_indentation_up;
		
		-- reference (serves as key in list of components)
		log ("component " & to_string (type_components.key(component)) & " properties");

		log_indentation_up;
		
		-- CS: library file name
		-- name in library
		log ("name in library "
			& et_libraries.to_string (type_components.element(component).name_in_library));
		
		-- value
		log ("value "
			& et_libraries.type_component_value.to_string (type_components.element(component).value));

		-- commissioned
		log ("commissioned "
			& string (type_components.element(component).commissioned));

		-- updated
		log ("updated      "
			& string (type_components.element(component).updated));

		-- author
		log ("author "
			& et_libraries.type_person_name.to_string (type_components.element(component).author));
		
		-- appearance
		log (to_string (type_components.element(component).appearance));

		-- depending on the component appearance there is more to report:
		case type_components.element(component).appearance is
			when sch_pcb =>

				-- package variant
				log (et_libraries.to_string (type_components.element(component).variant.variant));
				-- NOTE: This displays the type_component_variant (see et_libraries.ads).
				-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
				-- like in TL084D or TL084N.

				-- datasheet
				log ("datasheet "
					& et_libraries.type_component_datasheet.to_string (type_components.element(component).datasheet));

				-- partcode
				log ("partcode "
					& et_libraries.type_component_partcode.to_string (type_components.element(component).partcode));
				
				-- function
				log ("purpose "
					& et_libraries.type_component_purpose.to_string (type_components.element(component).purpose));

				-- bom
				log ("bom "
					& et_libraries.to_string (type_components.element (component).bom));

				
			when pcb => null; -- CS
			when others => null; -- CS should never happen as virtual components do not have a package
		end case;

		log_indentation_down;
		log_indentation_down;
		
	end write_component_properties;

	function component_reference (cursor : in type_components.cursor) 
		return type_component_reference is
	-- Returns the component reference where cursor points to.
	begin
		return type_components.key (cursor);
	end component_reference;

	function component_appearance (cursor : in type_components.cursor)
	-- Returns the component appearance where cursor points to.
		return type_component_appearance is
	begin
		return type_components.element (cursor).appearance;
	end component_appearance;

	function bom (cursor : in type_components.cursor)
	-- Returns the component bom status where cursor points to.
		return type_bom is
		b : type_bom; -- the bom status
	begin
		-- Only real components have a bom status.
		if component_appearance (cursor) = sch_pcb then
			b := type_components.element (cursor).bom;
		end if;
		return b;
	end bom;
	
	function component_power_flag (cursor : in type_components.cursor)
	-- Returns true if the component is a power flag.
		return boolean is
		use et_string_processing;
	begin
		-- Only vitual components have the power flag property. 
		-- For real components the return is always false;
		if type_components.element (cursor).appearance = sch then
			--log ("virtual component");
			--if type_components.element (cursor).power_flag then
			--	log ("power flag on");
			--else
			--	log ("power flag off");
			--end if;
			return type_components.element (cursor).power_flag;
		else
			--log ("real component");
			return false;
		end if;
	end component_power_flag;
	
	function component_name_in_library (cursor : in type_components.cursor) 
		return et_libraries.type_component_name.bounded_string is
	-- Returns the generic name of a component as it is listed in a library.
	-- The cursor must point to the component in question.
	begin
		return type_components.element (cursor).name_in_library;
	end component_name_in_library;


	function unit_exists (
	-- Returns true if the unit with the given name exists in the given list of units.
		name : in type_unit_name.bounded_string; -- the unit being inquired
		units : in et_schematic.type_units.map) -- the list of units
		return boolean is

		use et_schematic;
		use et_schematic.type_units;
	begin
		if et_schematic.type_units.find (container => units, key => name) = type_units.no_element then
			return false;
		else	
			return true;
		end if;
	end unit_exists;
	
	function position_of_unit (
	-- Returns the coordinates of the unit with the given name.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name : in type_unit_name.bounded_string; -- the unit being inquired
		units : in et_schematic.type_units.map) -- the list of units
		return et_coordinates.type_coordinates is
		unit_cursor : et_schematic.type_units.cursor;
	begin
		unit_cursor := et_schematic.type_units.find (container => units, key => name);
		return et_schematic.type_units.element (unit_cursor).position;
	end position_of_unit;

	function mirror_style_of_unit (
	-- Returns the mirror style of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name : in type_unit_name.bounded_string; -- the unit being inquired
		units : in et_schematic.type_units.map) -- the list of units
		return et_schematic.type_mirror is
		unit_cursor : et_schematic.type_units.cursor;
	begin
		unit_cursor := et_schematic.type_units.find (container => units, key => name);
		return et_schematic.type_units.element (unit_cursor).mirror;
	end mirror_style_of_unit;

	function orientation_of_unit (
	-- Returns the orientation of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name : in type_unit_name.bounded_string; -- the unit being inquired
		units : in et_schematic.type_units.map) -- the list of units
		return type_angle is
		unit_cursor : et_schematic.type_units.cursor;
	begin
		unit_cursor := et_schematic.type_units.find (container => units, key => name);
		return et_schematic.type_units.element (unit_cursor).orientation;
	end orientation_of_unit;
	

	
	procedure write_unit_properties (unit : in type_units.cursor) is
	-- Writes the properties of the unit indicated by the given cursor.
		use et_string_processing;
		use et_coordinates;
	begin
		log_indentation_up;
		
		-- unit name
		log ("unit " 
			& et_libraries.type_unit_name.to_string (type_units.key(unit)) & " properties");

		log_indentation_up;
		
		-- alternative representation
		log ("alternative (deMorgan) representation " 
			& to_lower (et_schematic.type_alternative_representation'image (type_units.element(unit).alt_repres)));

		-- timestamp
		log ("timestamp " 
			& string (type_units.element (unit).timestamp));

		-- position
		log (to_string (type_units.element (unit).position));

		-- orientation or angle
		log (to_string (type_units.element (unit).orientation));

		-- mirror style
		log (to_string (type_units.element (unit).mirror));

		
		-- placeholders
		log ("placeholders");
		log_indentation_up;

			-- reference
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).reference);

			-- value
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).value);

			-- some placeholders exist depending on the component appearance
			case type_units.element(unit).appearance is
				when sch_pcb =>
					
					-- package/footprint
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).packge);

					-- datasheet
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).datasheet);

					-- purpose
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).purpose);
					
					-- partcode
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).partcode);

					-- bom
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).bom);

					
				when others => null;
			end case;

			-- commissioned
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).commissioned);

			-- updated
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).updated);

			-- author
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).author);

		log_indentation_down;
		log_indentation_down;
		log_indentation_down;		
	end write_unit_properties;

	function length (segment : in type_net_segment) return type_distance is
	-- Returns the length of the given net segment.
		len : type_distance;
		use et_string_processing;
	begin
		len := distance (segment.coordinates_start, segment.coordinates_end);
		log (text => "segment length " & et_coordinates.to_string (len) & "mm", level => 3);
		return len;
	end length;
	
	procedure write_coordinates_of_segment (segment : in type_net_segment) is
	-- Writes the start and end coordinates of a net segment.
		use et_string_processing;
		use et_coordinates;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		
		log ("start "
			& to_string (segment.coordinates_start)
			& " end " 
			& to_string (segment.coordinates_end),
			level => log_threshold
			);
		
		log_indentation_down;
	end write_coordinates_of_segment;

	function to_string (segment : in type_net_segment) return string is
	-- Returns the start and end coordinates of the given net segment.
	begin
		return ("start "
			& to_string (segment.coordinates_start)
			& " end " 
			& to_string (segment.coordinates_end));
	end to_string;
	
-- 	procedure write_coordinates_of_junction (junction : in type_net_junction) is
-- 	-- Writes the coordinates of a net junction.
-- 		use et_string_processing;
-- 		use et_coordinates;
-- 	
-- 		log_threshold : type_log_level := 1;
-- 	begin
-- 		log_indentation_up;
-- 		
-- 		log (to_string (junction.coordinates),
-- 			 level => log_threshold
-- 			); 
-- 		
-- 		log_indentation_down;
-- 	end write_coordinates_of_junction;			


	function junction_sits_on_segment (
	-- Returns true if the given junction sits on the given net segment.
		junction	: in type_net_junction;
		segment		: in type_net_segment'class) 
		return boolean is

		-- CS: clean up as in port_sits_on_segment
		
		zero : constant et_coordinates.type_distance := et_coordinates.zero_distance;
		sits_on_segment : boolean := false;
		d : et_geometry.type_distance_point_from_line;

		use et_geometry;
		use et_coordinates;

	begin
		-- calculate the shortes distance of point from line.
		d := distance_of_point_from_line (
			point 		=> type_2d_point (junction.coordinates),
			line_start	=> type_2d_point (segment.coordinates_start),
			line_end	=> type_2d_point (segment.coordinates_end),
			line_range	=> inside_end_points);

		if (not d.out_of_range) and d.distance = zero then
			sits_on_segment := true;
		end if;

		return sits_on_segment;
	end junction_sits_on_segment;
	
	
	function to_component_reference (
	-- Converts a string like "IC303" to a composite type_component_reference.
	-- If allow_special_character_in_prefix is given true, the first character
	-- is allowed to be a special character.
	-- NOTE: Leading zeroes in the id are removed.
		text_in : in string;
		allow_special_character_in_prefix : in boolean := false		
		) return type_component_reference is

		r : type_component_reference := (
				prefix => type_component_prefix.to_bounded_string(""),
				id => 0,
				id_width => 1);
	
		c : character;
		p : type_component_prefix.bounded_string;
	
		procedure invalid_reference is
			use et_string_processing;
		begin
			log (text => latin_1.lf & message_error & "invalid component reference '" & text_in & "'",
				console => true);
			
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use et_libraries.type_component_prefix;
	begin
		-- assemble prefix
		for i in text_in'first .. text_in'last loop
			c := text_in(i);
			
			case i is 
				-- The first character MUST be an upper case letter.
				-- If allow_special_charater_in_prefix then the first letter is
				-- allowed to be a special character. (kicad uses '#' for power symbols)
				when 1 => 
					case allow_special_character_in_prefix is
						when false =>
							if is_upper(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;

						when true =>
							if is_upper(c) or is_special(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;
					end case;
					
				-- Further characters are appended to prefix if they are upper case letters.
				-- If a upper-case-letter is found, the prefix is assumed as complete.
				-- A lower case letter will later be detetect when assembling the component id.
				when others =>
					if is_upper(c) then
						r.prefix := r.prefix & c;
					else
						--put("   prefix " & type_component_prefix.to_string(r.prefix));
						-- CS: check if allowed prefix
						d := i; -- d holds the position of the charcter after the prefix.
							-- d is requried when reading the component id. see below.
						exit;
					end if;
			end case;
		end loop;

		-- assemble id
		-- Start with the last character in text_in.
		-- Finish at the position d (that is the first digit after the last letter, see above).
		-- All the characters within this range must be digits.
		-- The significance of the digit is increased after each pass.
		for i in reverse d .. text_in'last loop
			c := text_in(i);
			
			if is_digit(c) then
				r.id := r.id + 10**digit * natural'value(1 * c);
			else
				invalid_reference;
			end if;

			digit := digit + 1; -- increase digit significance (10**0, 10**1, ...)
		end loop;

		-- Set the id width.
		-- It is the number of digits processed when the id was assembled (see above).
		-- Example: if the given string was IC002 then digit is 3.
		r.id_width := digit;
		
-- 		put_line(" id    " & natural'image(r.id));
-- 		put_line(" digits" & natural'image(r.id_width));
		
		return r;
	end to_component_reference;

	function to_string ( reference : in type_component_reference) return string is
	-- Returns the given component reference as string.
	-- Prepends leading zeros according to reference.id_width.
		id_width_wanted	: natural := reference.id_width;
	
		-- The width of the given id is obtained by converting the id to a string
		-- and then by measuring its length:
		id_width_given	: natural := trim(natural'image(reference.id),left)'length;

		-- Finally the number of zeros to prepend is the difference of wanted 
		-- and given digits:
		lz				: natural := id_width_wanted - id_width_given;
	begin
		case lz is
			when 0 => -- no leading zeroes
				return (type_component_prefix.to_string(reference.prefix) 
					& trim(natural'image(reference.id),left));
				
			when others => -- leading zeros required
				return (type_component_prefix.to_string(reference.prefix) 
					& lz * '0' & trim(natural'image(reference.id),left));
		end case;
	end to_string;


	function compare_reference ( left, right : in type_component_reference) return boolean is
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
	-- CS: needs verification !
		result : boolean := false;
		use et_libraries.type_component_prefix;
	begin
		-- First we compare the prefix.
		-- Example: If left is C201 and right is R4 then the result is true as C comes before R.

		if left.prefix < right.prefix then -- like C201 and R4
			result := true;
		elsif left.prefix > right.prefix then -- like R4 and C201
			result := false;
		elsif left.prefix = right.prefix then -- like IC33 and IC34

			-- If equal prefixes, we compare the id:
			if left.id < right.id then -- like 33 and 34
				result := true;
			else
				result := false; -- like 34 and 33
			end if;

		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end compare_reference;

	function equal_reference (left, right : in type_component_reference) return boolean is
	-- Returns true if left equals right.
	-- Example: if IC4 = IC4 then return true.
		result : boolean := false;
		use et_libraries.type_component_prefix;
	begin
		-- First we compare the prefix. If prefixes are equal, we compare the id.
		-- If either of them does not match, the result is set false.
		if left.prefix = right.prefix then -- like IC and IC

			if left.id = right.id then -- like 4 and 4
				result := true;
			else -- like 5 and 6
				result := false;
			end if;
			
		else -- like R and IC
			result := false; 
		end if;

		return result;
	end equal_reference;
	


	procedure first_module is
	-- Resets the module_cursor to the first module of the rig.
	begin
		module_cursor := rig.first;
		-- CS: exception handler in case given module does not exist
	end first_module;
	

	procedure set_module (
	-- Sets the active module. Leaves module_cursor pointing
	-- to the module.
		module_name : in et_coordinates.type_submodule_name.bounded_string) is
	begin
		module_cursor := rig.find (module_name);
		-- CS: exception handler in case given module does not exist
	end set_module;

	
	procedure add_module (
	-- Adds a module into the rig. Leaves module_cursor pointing
	-- to the module inserted last.
		module_name : in et_coordinates.type_submodule_name.bounded_string;
		module		: in type_module) is
		
		inserted : boolean := false;
	begin
		rig.insert (
			key			=> module_name,
			new_item	=> module,
			position	=> module_cursor,
			inserted	=> inserted
			);

		if not inserted then
			null; -- CS: error message
			raise constraint_error;
		end if;
	end add_module;


	procedure add_gui_submodule (
	-- Inserts a gui submodule in the module (indicated by module_cursor)
		name		: in et_coordinates.type_submodule_name.bounded_string;
		gui_sub_mod	: in et_schematic.type_gui_submodule) is

		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_gui_submodules.cursor;

			use et_string_processing;
		begin
			module.submodules.insert (
				key			=> name,
				new_item	=> gui_sub_mod,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then
				if log_level >= 1 then
					null; -- CS: write this procedure:
					--et_schematic.write_gui_submodule_properties (gui_sub_mod => cursor);
				end if;
			else -- not inserted. net already in module -> abort
				null; -- CS: 
				raise constraint_error;
			end if;
		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_gui_submodule;

	
	procedure add_sheet_header (
	-- Inserts a sheet header in the module (indicated by module_cursor).
		header	: in type_sheet_header;
		sheet	: in type_schematic_file_name.bounded_string) is

		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.sheet_headers.insert (
				key			=> sheet,
				new_item	=> header);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_header
			end if;

		end add;

	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_sheet_header;

	
	procedure add_frame (
	-- Inserts a drawing frame in the the module (indicated by module_cursor).
	-- As drawing frames are collected in a simple list, the same frame
	-- can be added multiple times.
		frame	: in et_schematic.type_frame) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.frames.append (
				new_item	=> frame);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_frame_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_frame;

	
	
	procedure add_title_block (
	-- Inserts a title block in the the module (indicated by module_cursor).
	-- As title blocks are collected in a simple list, the same title block
	-- can be added multiple times.
		tblock	: in et_schematic.type_title_block) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.title_blocks.append (
				new_item	=> tblock);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_title_block_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_title_block;


	procedure add_note (
	-- Inserts a note in the the module (indicated by module_cursor).
	-- As notes are collected in a simple list, the same note
	-- can be added multiple times.
		note	: in et_schematic.type_note) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.notes.append (
				new_item	=> note);

			if log_level >= 1 then
				null; -- CS: 
				--et_schematic.write_note_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_note;

	function to_string (mirror : in type_mirror) return string is
	-- returns the given mirror style as string
	begin
		return "mirror style " & to_lower (type_mirror'image (mirror));
	end to_string;
	
	procedure add_strand (
	-- Adds a strand into the the module (indicated by module_cursor).
		strand : in et_schematic.type_strand) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is
			use et_string_processing;
		begin
			log_indentation_up;
			log (text => "inserting strand " & to_string (strand.name) & " in database ...", level => 2);
			log_indentation_down;

			module.strands.append (strand);
		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_strand;

	function first_strand return type_strands.cursor is
	-- Returns a cursor pointing to the first strand of the module (indicated by module_cursor).
		cursor : type_strands.cursor;	

		procedure set_cursor (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
 		begin
			cursor := module.strands.first;
		end set_cursor;
	
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> set_cursor'access
			);
		return cursor;
	end first_strand;

	procedure rename_strands (
	-- Renames all strands with the name_before to the name_after.
		name_before : type_net_name.bounded_string;
		name_after	: type_net_name.bounded_string) is

		use et_string_processing;
		
		procedure rename (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_schematic.type_strands;
			
			cursor : type_strands.cursor := module.strands.first;
			-- Points to the strand being processed
			
			renamed : boolean := false; -- signals that a strand renaming took place
			-- Used to abort renaming after an anonymous strands has been renamed.
			-- The names of anonymous strands like (N$6) are unique. So after the first
			-- renaming the procedure comes to an early end.

			procedure do_it (strand : in out type_strand) is
			-- Renames the strand if its name equals name_before.
			begin
				if strand.name = name_before then

					log_indentation_up;
					log (
						text => to_string (strand.name) 
							& " to "
							& to_string (name_after) & " ...",
							level => 2
						);
					log_indentation_down;

					strand.name := name_after; -- assign new name to strand

					renamed := true; -- signal renaming took place
				end if;
			end do_it;
			
		begin -- rename
			while cursor /= type_strands.no_element loop
				module.strands.update_element (position => cursor, process => do_it'access);

				-- Exit prematurely if name_before was anonymous. anonymous strand names are unique.
				-- So it is ok to exit prematurely.
				if renamed and anonymous (name_before) then
					exit;
				end if;
				
				next (cursor);
			end loop;
		end rename;
		
	begin -- rename_strands
		log ("renaming strands ...", level => 2);
		
		rig.update_element (
			position	=> module_cursor,
			process		=> rename'access
			);
	end rename_strands;

	
	function first_segment (cursor : in type_strands.cursor) return type_net_segments.cursor is
	-- Returns a cursor pointing to the first net segment of the given strand.
		segment_cursor : type_net_segments.cursor;

		procedure set_cursor (
			strand : in type_strand) is
		begin
			segment_cursor := strand.segments.first;
		end set_cursor;

	begin
		type_strands.query_element (
			position	=> cursor,
			process		=> set_cursor'access
			);
		return segment_cursor;
	end first_segment;

	function first_component return type_components.cursor is
	-- Returns a cursor pointing to the first component of the module (indicated by module_cursor).
		cursor : type_components.cursor;

		procedure set_cursor (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
 		begin
			cursor := module.components.first;
		end set_cursor;

	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> set_cursor'access
			);
		return cursor;
	end first_component;

	
	
	
	procedure add_component (
	-- Adds a component into the the module (indicated by module_cursor).
	-- If a component is already in the list, nothing happens.
	-- Components may occur multiple times in the schematic if they 
	-- consist of more than one unit.
	-- CS: This assumption may not apply for all CAE systems. Currently we
	-- consider only kicad. In other cases the "inserted" check (see below) 
	-- must be enabled via an argument.
		reference	: in et_libraries.type_component_reference;
		component	: in type_component) is
		
		procedure add (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_components.cursor;

			use et_string_processing;
		begin
			module.components.insert (
				key			=> reference,
				new_item	=> component,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

-- 			if inserted then -- first occurence of component
				if log_level >= 1 then
					et_schematic.write_component_properties (component => cursor);
				end if;
-- 			else -- not inserted
-- 				null; -- CS: see comment above
				--raise constraint_error;
-- 			end if;
		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_component;
	
	procedure add_unit ( -- CS: comments
		reference	: in et_libraries.type_component_reference;
		unit_name	: in et_libraries.type_unit_name.bounded_string;
		unit 		: in type_unit) is

		procedure add (
			reference	: in et_libraries.type_component_reference;
			component	: in out type_component) is

			inserted	: boolean := false;
			cursor		: type_units.cursor;

			use et_string_processing;
		begin
			component.units.insert (
				key			=> unit_name,
				new_item	=> unit,
				position	=> cursor, -- updates unit_cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then -- fine. unit was inserted successfully
				if log_level >= 1 then				
					write_unit_properties (unit => cursor);
				end if;
			else -- not inserted, unit already in component -> failure
				log_indentation_reset;
				log (
					text => message_error & "multiple occurence of the same unit !",
					console => true);
				raise constraint_error;
			end if;
		end add;
		
		procedure locate_component (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			cursor : type_components.cursor;
		begin
			cursor := module.components.find (reference);
			-- CS: do something if reference not found
			
			module.components.update_element (
				position	=> cursor,
				process		=> add'access
				);
		end locate_component;
		
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> locate_component'access
			);
	end add_unit;

	procedure reset_component_cursor (cursor : in out type_components.cursor) is
	-- Resets the given component cursor to the begin of the component list.
		procedure reset (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_components;
		begin
			cursor := first (module.components);
		end reset;
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> reset'access
			);
	end reset_component_cursor;

	procedure reset_library_cursor (cursor : in out type_full_library_names.cursor) is
	-- Resets the given library cursor to the begin of the library list list.
		procedure reset (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_full_library_names;
		begin
			cursor := first (module.libraries);
		end reset;
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> reset'access
			);
	end reset_library_cursor;

	function number_of_libraries return count_type is
	-- Returns the number of project libraries.
		n : count_type := 0;

		procedure get (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_full_library_names;
		begin
			n := length (module.libraries);
		end get;

	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end number_of_libraries;

	function units_of_component (component_cursor : in type_components.cursor) return type_units.map is
	-- Returns the units of the given component.
		u : type_units.map;

		procedure locate (
			name : in type_component_reference;
			component : in type_component) is
		begin
			-- copy the units of the component to the return value
			u := component.units;
		end locate;
		
	begin
		-- locate the given component by component_cursor
		type_components.query_element (component_cursor, locate'access);
		
		-- CS: do something if cursor invalid. via exception handler ?
		return u;
	end units_of_component;


-- 	procedure warning_on_name_less_net (
-- 	-- Writes a warning about a name-less net.
-- 		name 	: in et_schematic.type_net_name.bounded_string;
-- 		net		: in et_schematic.type_strand
-- 		) is
-- 
-- 		use et_string_processing;
-- 	begin
-- 		log (
-- 			text => message_warning & "name-less net " & to_string (name) & " found !"
-- 			);
-- 		-- CS: output coordinates of net (lowest x/Y)
-- 	end warning_on_name_less_net;
	

	
-- BOM
	
	procedure make_bom is
	-- Generates a bom file. This file is csv formatted and is to be processed by
	-- other ERP tools (like stock_manager, see <https://github.com/Blunk-electronic/stock_manager>)
		use ada.directories;
		use et_general;
		use et_string_processing;
		use et_export;
		use et_csv;
		use type_rig;
		use type_components;
		
		bom_file_name : type_bom_file_name.bounded_string;
		bom_handle : ada.text_io.file_type;

		column_component	: constant string (1 .. 9) := "COMPONENT";
		column_value		: constant string (1 .. 5) := "VALUE";
		column_generic_name	: constant string (1 ..12) := "GENERIC_NAME";
		column_package		: constant string (1 .. 7) := "PACKAGE";
		column_author		: constant string (1 .. 6) := "AUTHOR";
		column_bom			: constant string (1 .. 3) := "BOM";
		column_commissioned	: constant string (1 ..12) := "COMMISSIONED";
		column_purpose		: constant string (1 .. 7) := "PURPOSE";
		column_part_code	: constant string (1 .. 9) := "PART_CODE"; -- CS: make sure stock_manager can handle it. former PART_CODE_BEL
		column_part_code_ext: constant string (1 ..13) := "PART_CODE_EXT"; -- not used
		column_updated		: constant string (1 .. 7) := "UPDATED";

		component_cursor : type_components.cursor;
		
	begin
		first_module;
		
		log (text => "writing BOM ...", level => 1);

		while module_cursor /= type_rig.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (module_cursor)), level => 1);

			-- compose the netlist file name and its path like "../ET/motor_driver/CAM/motor_driver.net"
			bom_file_name := type_bom_file_name.to_bounded_string 
				(
				compose (
					containing_directory => compose 
						(
						containing_directory => compose (work_directory, to_string (key (module_cursor))),
						name => et_export.directory_cam
						),
					name => to_string (key (module_cursor)),
					extension => extension_bom)
				);

			-- create the netlist (which inevitably and intentionally overwrites the previous file)
			log_indentation_up;
			log (text => "creating BOM file " & type_bom_file_name.to_string (bom_file_name), level => 1);
			create (
				file => bom_handle,
				mode => out_file, 
				name => type_bom_file_name.to_string (bom_file_name));

			-- CS: A nice header should be placed. First make sure stock_manager can handle it.

			-- write the BOM table header
			put_field (file => bom_handle, text => column_component);
			put_field (file => bom_handle, text => column_value);
			put_field (file => bom_handle, text => column_generic_name);
			put_field (file => bom_handle, text => column_package);
			put_field (file => bom_handle, text => column_author);
			put_field (file => bom_handle, text => column_bom);
			put_field (file => bom_handle, text => column_commissioned);
			put_field (file => bom_handle, text => column_purpose);
			put_field (file => bom_handle, text => column_part_code);
			put_field (file => bom_handle, text => column_part_code_ext);
			put_field (file => bom_handle, text => column_updated);
			put_lf    (file => bom_handle);

			log_indentation_up;
			component_cursor := first_component;
			while component_cursor /= type_components.no_element loop

				-- We ignore all virtual components like power flags, power symbols, ...
				if component_appearance (component_cursor) = sch_pcb then

					if bom (component_cursor) = YES then
						log (text => to_string (key (component_cursor)), level => 1);

						put_field (file => bom_handle, text => to_string (key (component_cursor)));
						put_field (file => bom_handle, text => to_string (element (component_cursor).value));
						put_field (file => bom_handle, text => to_string (element (component_cursor).name_in_library));
						put_field (file => bom_handle, text => to_string (element (component_cursor).variant.variant.packge));
						put_field (file => bom_handle, text => to_string (element (component_cursor).author));
						put_field (file => bom_handle, text => to_string (element (component_cursor).bom));
						put_field (file => bom_handle, text => to_string (element (component_cursor).commissioned));
						put_field (file => bom_handle, text => to_string (element (component_cursor).purpose));
						put_field (file => bom_handle, text => to_string (element (component_cursor).partcode));

						-- CS: This is an empty field. it is reserved for the attribute "PART_CODE_EXT" 
						-- which is currently not supported:
						put_field (file => bom_handle, text => "");

						put_field (file => bom_handle, text => to_string (element (component_cursor).updated));
						put_lf    (file => bom_handle);

					end if;
				end if;

				next (component_cursor);
			end loop;
			log_indentation_down;
			
			-- CS: A list end mark should be placed. First make sure stock_manager can handle it.
			-- put_line (bom_handle, comment_mark & " end of list");
			
			close (bom_handle);
			log_indentation_down;
			next (module_cursor);
		end loop;
		
	end make_bom;

	

-- STATISTICS

	function components_total return count_type is
	-- Returns the total number of components (inc. virtual components) in the module indicated by module_cursor.
		n : count_type := 0;

		procedure get (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_components;
		begin
			n := length (module.components);
		end get;
	
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end components_total;

	function components_real (mounted_only : in boolean := false) return count_type is
	-- Returns the number of real components in the module indicated by module_cursor.
	-- If mounted_only is true, only components with the flag "bom" set are adressed.
	-- If mounted_only is false, all real components are adressed.
		n : count_type := 0;

		procedure get (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is

			component : type_components.cursor;
			use type_components;
			use et_string_processing;
		begin -- get
			if mounted_only then
				log (text => "mounted components", level => 1);
			else
				log (text => "real components", level => 1);
			end if;

			component := module.components.first;			
			while component /= type_components.no_element loop

				-- filter real components
				if component_appearance (component) = sch_pcb then
					log_indentation_up;
					
					if mounted_only then
						if bom (component) = YES then
							log (text => to_string (key (component)), level => 1);
							n := n + 1;
						end if;
					else
						log (text => to_string (key (component)), level => 1);
						n := n + 1;
					end if;

					log_indentation_down;
				end if;
				next (component);
			end loop;
		end get;
	
	begin -- components_real
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end components_real;

	function components_virtual return count_type is
	-- Returns the number of virtual components in the module indicated by module_cursor.
		n : count_type := 0;

		procedure get (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is

			component : type_components.cursor;
			use type_components;
			use et_string_processing;
		begin
			log (text => "virtual components", level => 1);
			
			component := module.components.first;
			while component /= type_components.no_element loop
				
				-- filter real components
				if component_appearance (component) = sch then
					log_indentation_up;
					
					log (text => to_string (key (component)), level => 1);
					n := n + 1;
					
					log_indentation_down;
				end if;
				next (component);
			end loop;
			
		end get;
	
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end components_virtual;

	
	procedure make_statistics is
	-- Generates the statistics on components and nets of the rig.
	-- Breaks statistics up into submodules, general statistics (CAD) and CAM related things.
		statistics_file_name_cad: type_statistic_file_name.bounded_string;
		statistics_file_name_cam: type_statistic_file_name.bounded_string;
		statistics_handle_cad	: ada.text_io.file_type;
		statistics_handle_cam	: ada.text_io.file_type;

		component : type_components.cursor;
		-- CS net
		
		use ada.directories;
		use et_general;
		use type_components;
		use type_rig;
		use et_string_processing;
		use et_netlist;
	begin
		first_module;
		
		log (text => "writing statistics ...", level => 1);

		while module_cursor /= type_rig.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (module_cursor)), level => 1);
			log_indentation_up;

			-- CAD
			-- compose the CAD statistics file name and its path like "../ET/motor_driver/motor_driver.stat"
			statistics_file_name_cad := type_statistic_file_name.to_bounded_string 
				(
				compose (
					containing_directory => compose (work_directory, to_string (key (module_cursor))),
					name => to_string (key (module_cursor)),
					extension => extension_statistics)
				);

			-- create the statistics file (which inevitably and intentionally overwrites the previous file)
			log (text => "CAD statistics file " & type_statistic_file_name.to_string (statistics_file_name_cad), level => 1);
			create (
				file => statistics_handle_cad,
				mode => out_file, 
				name => type_statistic_file_name.to_string (statistics_file_name_cad));

			log_indentation_up;
			put_line (statistics_handle_cad, comment_mark & " " & system_name & " CAD statistics");
			put_line (statistics_handle_cad, comment_mark & " date " & string (date_now));
			put_line (statistics_handle_cad, comment_mark & " module " & to_string (key (module_cursor)));
			put_line (statistics_handle_cad, comment_mark & " " & row_separator_double);

			-- components
			put_line (statistics_handle_cad, "components");
			put_line (statistics_handle_cad, " total  " & count_type'image (components_total) & " (incl. virtual components)");
			put_line (statistics_handle_cad, " real   " & count_type'image (components_real)); -- all real components ! Regardless of bom status !
			put_line (statistics_handle_cad, " virtual" & count_type'image (components_virtual) & " (power symbols, power flags, ...)");
			new_line (statistics_handle_cad);
			-- CS: resitors, leds, transitors, ...

			-- nets
			put_line (statistics_handle_cad, "nets");
			-- CS: currently we get the net and pin numbers from et_netlist.rig.
			-- In the future this data should be taken from et_schematic.rig.
			et_netlist.set_module (key (module_cursor));
			put_line (statistics_handle_cad, " total  " & count_type'image (et_netlist.net_count));
			-- As for the total number of ports, we take all ports into account as they are listed in et_netlist.rig.
			-- This includes ports of virtual components except so called "power_flags".
			put_line (statistics_handle_cad, "  ports " & count_type'image (et_netlist.component_ports_total) & " (excl. power flags)");
			
			-- finish statistics			
			put_line (statistics_handle_cad, comment_mark & " " & row_separator_single);
			put_line (statistics_handle_cad, comment_mark & " end of list");
			log_indentation_down;
			close (statistics_handle_cad);



			-- CAM
			-- compose the CAM statistics file name and its path like "../ET/motor_driver/CAM/motor_driver.stat"
			statistics_file_name_cam := type_statistic_file_name.to_bounded_string 
				(
				compose (
					containing_directory => compose 
						(
						containing_directory => compose (work_directory, to_string (key (module_cursor))),
						name => et_export.directory_cam
						),
					name => to_string (key (module_cursor)),
					extension => extension_statistics)
				);

			-- create the statistics file (which inevitably and intentionally overwrites the previous file)
			log (text => "CAM statistics file " & type_statistic_file_name.to_string (statistics_file_name_cam), level => 1);
			create (
				file => statistics_handle_cam,
				mode => out_file, 
				name => type_statistic_file_name.to_string (statistics_file_name_cam));

			log_indentation_up;
			put_line (statistics_handle_cam, comment_mark & " " & system_name & " CAM statistics");
			put_line (statistics_handle_cam, comment_mark & " date " & string (date_now));
			put_line (statistics_handle_cam, comment_mark & " module " & to_string (key (module_cursor)));
			put_line (statistics_handle_cam, comment_mark & " " & row_separator_double);

			-- components
			put_line (statistics_handle_cam, "components");
			put_line (statistics_handle_cam, " total" & count_type'image (components_real (mounted_only => true)));
			-- As for the total number of real component ports, we take all ports into account for which a physical
			-- pad must be manufactured. Here it does not matter if a component is to be mounted or not, if a pin is connected or not.
			-- CS: THT/SMD
			-- CS: THT/SMD/pins/pads
			-- CS: resitors, leds, transitors, ...
			new_line (statistics_handle_cam);

			-- nets
			put_line (statistics_handle_cam, "nets");
			-- CS: currently we get the net and pin numbers from et_netlist.rig.
			-- In the future this data should be taken from et_schematic.rig.
			et_netlist.set_module (key (module_cursor));
			put_line (statistics_handle_cam, " total" & count_type'image (et_netlist.net_count));

			-- finish statistics
			put_line (statistics_handle_cam, comment_mark & " " & row_separator_single);
			put_line (statistics_handle_cam, comment_mark & " end of list");
			log_indentation_down;
			close (statistics_handle_cam);



			
			log_indentation_down;
			next (module_cursor);
		end loop;
			
		
	end make_statistics;

	
end et_schematic;
-- Soli Deo Gloria

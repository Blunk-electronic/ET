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

--   For correct displaying set tab with in your edtito to 4.

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
	
	-- Here we append a submodule name to the path_to_submodule.
	-- CS: unify with procedure delete_last_module_name_from_path
	procedure append_name_of_parent_module_to_path (submodule : in et_coordinates.type_submodule_name.bounded_string) is
		use et_string_processing;
		use ada.directories;
		use et_coordinates.type_submodule_name;
	begin
		-- CS: limit path length !
-- 		log ("append path_to_submodule " 
-- 			& base_name (type_submodule_name.to_string (submodule)), level => 1);

		-- Since we are dealing with file names, the extension must be removed before appending.
		type_path_to_submodule.append (path_to_submodule,
			to_bounded_string (base_name (type_submodule_name.to_string (submodule))));

	end append_name_of_parent_module_to_path;
	
	-- Here we remove the last submodule name form the path_to_submodule.
	procedure delete_last_module_name_from_path is
		use et_coordinates;
	begin
		type_path_to_submodule.delete_last (path_to_submodule);
	end delete_last_module_name_from_path;

	function to_submodule_name (file_name : in type_schematic_file_name.bounded_string)
		return et_coordinates.type_submodule_name.bounded_string is
	-- Retruns the base name of the given schematic file name as submodule name.
		use ada.directories;
	begin
		-- CS: test if given submodule has an extension. if not return
		-- submodule as it is.
		--return to_bounded_string (base_name (et_coordinates.to_string (submodule)));
		return type_submodule_name.to_bounded_string (base_name (to_string (file_name)));
	end to_submodule_name;
	
	function to_string (net_name : in type_net_name.bounded_string) return string is
	-- Returns the given net name as string.
	begin
		return type_net_name.to_string (net_name);
	end to_string;

	function simple_name (net_name : in type_net_name.bounded_string) return type_net_name.bounded_string is
	-- Returns the simple name of the given net name.
	-- Example: If the given name is "MOTOR_DRIVER.CLOCK" then the return is "CLOCK".
		position_of_last_separator : natural := 0;
		name : type_net_name.bounded_string;
	begin
		-- Detect position of last hierarchy separator.
		position_of_last_separator := index (net_name, hierarchy_separator, backward);

		-- If the given net name is a simple name already, return it as it is.
		-- Otherwise extract the simple net name from position of hierarchy 
		-- separator until end of net_name.
		if position_of_last_separator > 0 then
			name := type_net_name.to_bounded_string (
				slice (net_name, position_of_last_separator + 1, length (net_name)));
		else
			name := net_name;
		end if;
		
		return name;
	end simple_name;
	
	function anonymous (net_name : in type_net_name.bounded_string) return boolean is
	-- Returns true if the given net name is anonymous.
	begin
		-- CS: this is just a test if anonymous_net_name_prefix is somewhere
		-- in the net name. Should be improved.
		if type_net_name.count (net_name, anonymous_net_name_prefix) > 0 then
			return true;
		else 
			return false;
		end if;
	end anonymous;
	
	procedure write_label_properties (label : in type_net_label) is
	-- Writes the properties of the given net label in the logfile.
		use et_string_processing;
		use et_coordinates;

		log_threshold : type_log_level := 2;
	begin
		log_indentation_up;
		
		case label.label_appearance is
			when simple =>
				log (text => "simple label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
				
			when tag =>
				if label.hierarchic then
					log (text => "hierarchic label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
				end if;
				if label.global then
					log (text => "global label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
				end if;
					-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_up;
		log (text => to_string (label.orientation), level => log_threshold + 1);
		
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

	function to_string (label : in type_net_label; scope : in type_scope) return string is
	-- Returns the coordinates of the given label as string.
	begin
		return (to_string (position => label.coordinates, scope => scope));
	end to_string;
	
	procedure write_junction_properties (junction : in type_net_junction) is
	-- Writes the properties of the given net junction in the logfile.
		use et_string_processing;
		use et_coordinates;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		log ("net junction");
		log_indentation_up;
		log (to_string (position => junction.coordinates), log_threshold);
		log_indentation_down;
		log_indentation_down;		
	end write_junction_properties;

	function to_string (junction : in type_net_junction; scope : in type_scope) return string is
	-- Returns the position of the given junction as string.
	begin	
		return (to_string (position => junction.coordinates, scope => scope));
	end to_string;
	
	procedure write_note_properties (
		note : in et_schematic.type_note;
		log_threshold : in et_string_processing.type_log_level := 0) is
	-- Writes the properties of the given note
		use et_string_processing;
		use et_coordinates;
	
-- 		log_threshold : type_log_level := 2;
	begin
		log_indentation_up;
		log ("text note", log_threshold);

		log_indentation_up;

		-- content
		if et_libraries.type_text_content.length (note.content) > 0 then
			log (text => "content '" & type_text_content.to_string (note.content) & "'", level => log_threshold);
		else
			log (text => et_string_processing.message_warning & "no content !", level => log_threshold); 
		end if;

		
		if log_level >= log_threshold + 1 then
			
			-- position
			log (to_string (position => note.coordinates));
			
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
	
	procedure write_component_properties (
	-- Writes the properties of the component indicated by the given cursor.
		component : in type_components.cursor;
		log_threshold : in et_string_processing.type_log_level) is

		use et_string_processing;
	begin
		log_indentation_up;
		
		-- reference (serves as key in list of components)
		log ("component " & to_string (type_components.key (component)) & " properties", log_threshold);

		log_indentation_up;
		
		-- CS: library file name
		-- name in library
		log ("name in library "
			& to_string (type_components.element (component).name_in_library), log_threshold);
		
		-- value
		log ("value "
			& to_string (type_components.element (component).value), log_threshold);

		-- commissioned
		log ("commissioned "
			& string (type_components.element (component).commissioned), log_threshold);

		-- updated
		log ("updated      "
			& string (type_components.element(component).updated), log_threshold);

		-- author
		log ("author "
			& to_string (type_components.element(component).author), log_threshold);
		
		-- appearance
		log (to_string (type_components.element(component).appearance), log_threshold);

		-- depending on the component appearance there is more to report:
		case type_components.element(component).appearance is
			when sch_pcb =>

				-- package variant
				log (to_string (type_components.element (component).variant.variant), log_threshold);
				-- NOTE: This displays the type_component_variant (see et_libraries.ads).
				-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
				-- like in TL084D or TL084N.

				-- datasheet
				log ("datasheet "
					& type_component_datasheet.to_string (type_components.element (component).datasheet), log_threshold);

				-- partcode
				log ("partcode "
					& type_component_partcode.to_string (type_components.element (component).partcode), log_threshold);
				
				-- purpose
				log ("purpose "
					& type_component_purpose.to_string (type_components.element(component).purpose), log_threshold);

				-- bom
				log ("bom "
					& to_string (type_components.element (component).bom), log_threshold);

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
		return type_appearance_schematic is
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
	

	
	procedure write_unit_properties (
	-- Writes the properties of the unit indicated by the given cursor.
		unit			: in type_units.cursor;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;
		use et_coordinates;
	begin
		log_indentation_up;
		
		-- unit name
		log ("unit " 
			& to_string (type_units.key (unit)), log_threshold);

		log_indentation_up;
		
		-- alternative representation
		log ("alternative (deMorgan) representation " 
			 & to_lower (type_alternative_representation'image (type_units.element (unit).alt_repres)),
			 log_threshold + 1);

		-- timestamp
		log ("timestamp " 
			& string (type_units.element (unit).timestamp), log_threshold + 1);

		-- position
		log (to_string (position => type_units.element (unit).position), log_threshold + 1);

		-- orientation or angle
		log (to_string (type_units.element (unit).orientation), log_threshold + 1);

		-- mirror style
		log (to_string (type_units.element (unit).mirror), log_threshold + 1);

		
		-- placeholders
		log ("placeholders", log_threshold + 1);
		log_indentation_up;

			-- reference
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).reference,
				log_threshold => log_threshold + 1);

			-- value
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).value,
				log_threshold => log_threshold + 1);

			-- some placeholders exist depending on the component appearance
			case type_units.element (unit).appearance is
				when sch_pcb =>
					
					-- package/footprint
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).packge,
						log_threshold => log_threshold + 1);

					-- datasheet
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).datasheet,
						log_threshold => log_threshold + 1);

					-- purpose
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).purpose,
						log_threshold => log_threshold + 1);
					
					-- partcode
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).partcode,
						log_threshold => log_threshold + 1);

					-- bom
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).bom,
						log_threshold => log_threshold + 1);
					
				when others => null;
			end case;

			-- commissioned
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).commissioned,
				log_threshold => log_threshold + 1);

			-- updated
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).updated,
				log_threshold => log_threshold + 1);

			-- author
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).author,
				log_threshold => log_threshold + 1);

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
	
-- 	procedure write_coordinates_of_segment (segment : in type_net_segment) is
-- 	-- Writes the start and end coordinates of a net segment.
-- 		use et_string_processing;
-- 		use et_coordinates;
-- 	
-- 		log_threshold : type_log_level := 1;
-- 	begin
-- 		log_indentation_up;
-- 		
-- 		log ("start "
-- 			& to_string (position => segment.coordinates_start)
-- 			& " end " 
-- 			& to_string (position => segment.coordinates_end),
-- 			level => log_threshold
-- 			);
-- 		
-- 		log_indentation_down;
-- 	end write_coordinates_of_segment;

	function to_string (segment : in type_net_segment; scope : in type_scope := sheet) return string is
	-- Returns the start and end coordinates of the given net segment.
	begin
		-- CS: it is sufficient to output
		return ("start "
			& to_string (position => segment.coordinates_start, scope => scope)
			& " end " 
			& to_string (position => segment.coordinates_end, scope => xy));
	end to_string;

	function to_string (scope : in type_strand_scope) return string is
	-- Retruns the given scope as string.
	begin
		--return to_lower (type_scope_of_net'image (scope));
		return type_strand_scope'image (scope);
	end to_string;

	function lowest_xy (
	-- Returns the lowest x/y position of the given strand.
		strand : in type_strand;
		log_threshold : in et_string_processing.type_log_level
		) return type_2d_point is
		point_1, point_2 : type_2d_point;
		segment : type_net_segments.cursor;
	
		use type_net_segments;
		use et_string_processing;

		-- CS: usage of intermediate variables for x/Y of start/end points could improve performance
	begin
		log_indentation_up;
		log ("calculating the point nearest to drawing origin ...", log_threshold);

		-- init point_1 as the farest possible point from drawing origin
		set_x (point_1, type_distance_xy'last);
		set_y (point_1, type_distance_xy'last);
		
		-- loop through segments and keep the nearest point to origin
		segment := strand.segments.first;
		while segment /= type_net_segments.no_element loop

			-- check start point of segment
			-- if closer to orign than point_1 keep start point
			point_2	:= type_2d_point (element (segment).coordinates_start);
			if distance (point_2, zero) < distance (point_1, zero) then
				log (" start", log_threshold + 1);
				point_1 := point_2;
			end if;

			-- check start point of segment
			-- if closer to orign than point_1 keep end point
			point_2	:= type_2d_point (element (segment).coordinates_end);
			if distance (point_2, zero) < distance (point_1, zero) then
				log (" end", log_threshold + 1);
				point_1 := point_2;
			end if;
			
			next (segment);
		end loop;

		log_indentation_down;
		
		return point_1;
	end lowest_xy;
	
	procedure add_strand (
	-- Adds a strand into the the module (indicated by module_cursor).
		strand : in et_schematic.type_strand) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is
			use et_string_processing;
		begin
			log_indentation_up;
			log (text => "inserting strand " & to_string (strand.name) & " in database ...", level => 3);
			log_indentation_down;

			module.strands.append (strand);
		end add;
		
	begin -- add_strand
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_strand;

	
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
                    --log ("hierachic sheet", console => true);
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

    function first_gui_submodule return type_gui_submodules.cursor is
    -- Returns a cursor pointing to the first gui_submodule of the moduel (indicated by module_cursor)
		cursor : type_gui_submodules.cursor;	

		procedure set_cursor (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
 		begin
			cursor := module.submodules.first;
		end set_cursor;
	
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> set_cursor'access
			);
		return cursor;
	end first_gui_submodule;

	
	
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

-- PORTLISTS

	function build_portlists (log_threshold : in et_string_processing.type_log_level) return type_portlists.map is
	-- Returns a list of components with the absolute positions of their ports as they are placed in the schematic.
	-- This applies to the module indicated by module_cursor.
		
	-- Locates the components of the schematic in the libraries. 
	-- Computes the absolute port positions of components from:
	--  - the port coordinates provided by the librares
	--  - the unit coordinates provided by the schematic
	--  - the unit mirror style provided by the schematic
	--  - the unit orientation provided by the schematic
	-- Special threatment for "common to all units" ports of global units. See comments.
	
	-- Stores the absolute port coordinates in map "portlists". 
	-- The key into this map is the component reference.
	
	-- Saves the portlists in the module (indicated by module_cursor).
	
		-- Here we collect the portlists:
		portlists					: type_portlists.map;
		component_inserted			: boolean;
		component_cursor_portlists	: type_portlists.cursor; -- points to the portlist being built
	
		use et_libraries;
		use et_libraries.type_full_library_names;
		use et_schematic.type_components;
		use et_string_processing;

		-- This component cursor points to the schematic component being processed.
		component_cursor_sch: et_schematic.type_components.cursor;

		-- The generic name of a component in a library (like TRANSISTOR_PNP or LED) 
		-- is tempoarily held here:
		component_name		: et_libraries.type_component_name.bounded_string;

		-- The component reference in the schematic (like R44 or IC34)
		-- is tempoarily held here:
		component_reference	: et_libraries.type_component_reference;
	
		-- The library cursor points to the library to search in (in module.libraries).
		-- NOTE: module.libraries is just a list of full library names, no more.
		library_cursor_sch	: type_full_library_names.cursor;
		library_name		: type_full_library_name.bounded_string;

		-- This component cursor points to the library component being processed.
		use et_libraries.type_components;
		component_cursor_lib: et_libraries.type_components.cursor;

		-- CS: log_threshold for messages below

		-- For tempoarily storage of units of a component (taken from the schematic):
		units_sch : et_schematic.type_units.map;

		procedure extract_ports is
		-- Extracts the ports of the component indicated by component_cursor_lib.
		-- NOTE: The library contains the (x/y) positions of the ports.
			use et_libraries.type_units_internal;
			use et_libraries.type_ports;
			use et_coordinates;
			use et_schematic;
		
			-- The unit cursor of the component advances through the units stored in the library.
			unit_cursor_internal	: type_units_internal.cursor;

			-- The port cursor of the unit indicates the port of a unit.
			port_cursor				: et_libraries.type_ports.cursor; 

			unit_name_lib : type_unit_name.bounded_string; -- the unit name in the library. like "A", "B" or "PWR"
			unit_position : et_coordinates.type_coordinates; -- the coordinates of the current unit
			-- CS: external units

			procedure add_port is
			-- Builds a new port and appends it to portlist of the current 
			-- component (indicated by component_cursor_portlists).
			
			-- The library defined properties of the port are taken from where port_cursor points to.
			-- They are copied to the new port without change.
			
			-- Properites set in the schematic such as path, module name, sheet are copied into the
			-- new port unchanged. X and Y position of the port must be re-computed according to
			-- the rotation, mirror style and position of the unit in the schematic.
			-- NOTE: It is important first to rotate, then mirror (if required) and finally to move/offset it.

				procedure add (
					component	: in type_component_reference;
					ports		: in out type_ports.list) is
					use et_coordinates;
					
					port_coordinates : type_coordinates;

				begin -- add

					-- Init port coordinates with the coordinates of the port found in the library.
					-- The port position is a type_2d_point and must be converted to type_coordinates.
					et_coordinates.set (
						point		=> port_coordinates,
						position	=> to_coordinates (element (port_cursor).coordinates)); -- with type conversion

					-- rotate port coordinates
					rotate (
						point => port_coordinates,
						angle => et_schematic.orientation_of_unit (unit_name_lib, units_sch),
						log_threshold => log_threshold + 3);

					-- Mirror port coordinates if required.
					case mirror_style_of_unit (unit_name_lib, units_sch) is
						when none => null; -- unit not mirrored in schematic
						when x_axis => mirror (point => port_coordinates, axis => x);
						when y_axis => mirror (point => port_coordinates, axis => y);
					end case;

					-- offset port coordinates by the coordinates of the unit found in the schematic
					move (point => port_coordinates, offset => unit_position);

					-- path remains unchanged because the port is still where the unit is
					set_path (port_coordinates, path (unit_position));

					-- module name remains unchanged because the port is still in the same module
					set_module (port_coordinates, module (unit_position));

					-- sheet name remains unchanged because the sheet is still the same
					set_sheet (port_coordinates, sheet (unit_position));
					
					-- insert a the newly built port in the portlist of the component
					type_ports.append (
						container => ports,
						new_item => (

							-- library defined properites:
							--port		=> key (port_cursor), -- the port name
							port		=> element (port_cursor).name, -- the port name
							pin			=> element (port_cursor).pin, -- the pin name
							direction	=> element (port_cursor).direction, -- the port direction
							style		=> element (port_cursor).style, -- port style

							-- We also set the port appearance. Later when writing the netlist, this property
							-- serves to tell real from virtual ports.
							appearance	=> et_schematic.component_appearance (component_cursor_sch),

							-- schematic defined properties:
							coordinates	=> port_coordinates,

							processed	=> false -- used by netlist generator (procedure make_netlists)
							));

					log_indentation_up;
					log (to_string (last_element (ports).direction), log_threshold + 3);
					-- CS: other port properties
					log (to_string (position => last_element (ports).coordinates), log_threshold + 3);
					log_indentation_down;
				end add;
				
			begin -- add_port
				-- We update the portlist of the component in container portlists.
				-- The cursor to the portlist was set when the element got inserted (see below in procedure build_portlists).
				type_portlists.update_element (
					container	=> portlists,
					position	=> component_cursor_portlists,
					process		=> add'access);
			end add_port;

			procedure ports_of_global_unit is
			-- Searches in the component (indicated by component_cursor_lib) for units
			-- with the "global" flag set.
			-- Sets the port_cursor for each port and leaves the rest of the work to procedure add_port.
				unit_cursor : type_units_internal.cursor;
			begin
				-- Loop in list of internal units:
				unit_cursor := first_internal_unit (component_cursor_lib);
				while unit_cursor /= type_units_internal.no_element loop
					log_indentation_up;

					if element (unit_cursor).global then
						--log ("global unit " & to_string (key (unit_cursor)));

						-- NOTE: One could think of exiting the loop here once the global unit
						-- has been found. If it were about KiCad only, this would make sense
						-- as there can be only one global unit per component.
						-- As for other CAE tools there might be more global units, so there
						-- is no early exit here.

						-- Loop in port list of the unit:						
						port_cursor := first_port (unit_cursor); -- port in library
						while port_cursor /= et_libraries.type_ports.no_element loop

							--log ("port " & type_port_name.to_string (key (port_cursor))
							log (text => "port " & type_port_name.to_string (element (port_cursor).name)
									& " pin/pad " & to_string (element (port_cursor).pin),
								 level => log_threshold + 2);

							-- Build a new port and append port to portlist of the 
							-- current component (indicated by component_cursor_portlists).
							add_port;
							
							port_cursor := next (port_cursor);
						end loop;
					end if;

					log_indentation_down;
					unit_cursor := next (unit_cursor);
				end loop;

			end ports_of_global_unit;
			
		begin -- extract_ports
			-- Loop in unit list of the component (indicated by component_cursor_lib).
			-- unit_cursor_internal points to the unit in the library.
			-- Frequently, not all units of a component are deployed in the schematic.
			-- If a unit is not deployed it is ignored. Otherwise the coordinates of the
			-- unit in the schematic are stored in unit_position.

			-- Init the unit cursor of the current component:
			unit_cursor_internal := first_internal_unit (component_cursor_lib);

			-- Loop in list of internal units:
			while unit_cursor_internal /= type_units_internal.no_element loop
				log_indentation_up;

				-- get the unit name
				unit_name_lib := key (unit_cursor_internal);

				-- Now the unit name serves as key into the unit list we got from the schematic (unit_sch).
				-- If the unit is deployed in the schematic, we load unit_position. 
				-- unit_position holds the position of the unit in the schematic.
				if unit_exists (name => unit_name_lib, units => units_sch) then -- if unit deployed in schematic
					log ("unit " & to_string (unit_name_lib), log_threshold + 1);
					unit_position := position_of_unit (name => unit_name_lib, units => units_sch); -- pos. in schematic
					log_indentation_up;
					log (to_string (position => unit_position), log_threshold + 2);

					-- Get the ports of the current unit. Start with the first port of the unit.
					-- The unit_position plus the relative port position (in library) yields the absolute
					-- position of the port (in schematic).

					-- Init port cursor
					port_cursor := first_port (unit_cursor_internal); -- port in library

					-- Loop in port list of the unit:
					while port_cursor /= et_libraries.type_ports.no_element loop
						log_indentation_up;
						--log ("port " & type_port_name.to_string (key (port_cursor))
						log (text => "port " & type_port_name.to_string (element (port_cursor).name)
								& " pin/pad " & to_string (element (port_cursor).pin),
							 level => log_threshold + 2
							);
						
						-- Build a new port and append port to portlist of the 
						-- current component (indicated by component_cursor_portlists).
						add_port;
						
						log_indentation_down;
						port_cursor := next (port_cursor);
					end loop;

					-- SEARCH FOR PORTS OF GLOBAL UNITS. 
					
					-- NOTE: Take a rest before trying to understand the following:
					
					-- The problem with ports that are "common to all units" (KiCad terminology) is:
					--  The unit they belong to does not appear in the schematic, whereas their ports
					--  are visible on each unit (kicad button "show hidden pins").
					-- Solution: We assume all "common to all units" ports belong to all units of the 
					-- component, thus inheriting the unit_name_lib and the unti_position.
					-- The the unit_name_lib and unit_position of the current unit are applied
					-- to the global units.
					ports_of_global_unit;
					
					log_indentation_down;
				end if;

				log_indentation_down;
				unit_cursor_internal := next (unit_cursor_internal);
			end loop;
			
		end extract_ports;

		procedure check_appearance_sch_vs_lib is
		-- Verifies appearance of schematic component against library component.
		begin
			if et_schematic.component_appearance (component_cursor_sch) = 
			   et_libraries.component_appearance (component_cursor_lib) then
				null; -- fine
			else
				-- this should never happen
				log_indentation_down;
				log (text => message_error & "comonent appearance mismatch !", console => true);
				-- CS: provide more details on the affected component
				raise constraint_error;
			end if;
		end check_appearance_sch_vs_lib;

		procedure save_portlists is
		-- Save the portlists in the module (indicated by module_cursor).
		-- module_cursor points already there.
			use type_rig;
		
			procedure save (
				module_name : in type_submodule_name.bounded_string;
				module : in out type_module) is
			begin
				module.portlists := portlists;
			end save;
		begin
			log ("saving portlists ...", log_threshold + 1);
			update_element (
				container => rig,
				position => module_cursor,
				process => save'access);
		end save_portlists;
		
	begin -- build_portlists
		log_indentation_up;
		log (text => "building portlists ...", level => log_threshold);
		log_indentation_up;

		-- The library contains the coordinates of the ports whereas
		-- the schematic provides the coordinates of the units of a component.
		-- These coordinates summed up yields the absolute position of the ports.
		
		-- Loop in component list of schematic. component_cursor_sch points to the 
		-- particular component. 

		-- We ignore components that are "power_flags". Since the portlists are a prerequisite
		-- of netlist generation, this implies, that "power_flags" are not in the netlists.
		-- Yet other virtual components like
		-- power symbols like GND or P3V3 are relevant indeed. Because later when we do
		-- the netlist post-processing they enforce their port names to the connected net.
		
		-- For each component, store a list of its units in units_sch.
		-- This list contains the units found in the schematic with their coordinates.
		-- These coordinates plus the port coordinates (extracted in 
		-- procedure (extract_ports) will later yield the absolute positions of the ports.
		et_schematic.reset_component_cursor (component_cursor_sch);
		while component_cursor_sch /= et_schematic.type_components.no_element loop

			-- power flags are to be skipped
			if not et_schematic.component_power_flag (component_cursor_sch) then
		
				-- log component by its reference		
				component_reference :=  et_schematic.component_reference (component_cursor_sch);
				log ("reference " & et_schematic.to_string (component_reference), log_threshold + 1);
				
				-- Insert component in portlists. for the moment the portlist of this component is empty.
				-- After that the component_cursor_portlists points to the component. This cursor will
				-- later be used to add a port to the portlists.
				type_portlists.insert (
					container	=> portlists,
					key			=> component_reference, -- like R44
					new_item	=> type_ports.empty_list,
					inserted	=> component_inserted, -- obligatory, no further meaning
					position	=> component_cursor_portlists -- points to the portlist being built
					);
				
				-- get the units of the current schematic component (indicated by component_cursor_sch)
				units_sch := et_schematic.units_of_component (component_cursor_sch);
	
				-- get generic component name (as listed in a library)
				log_indentation_up;			
				component_name := et_schematic.component_name_in_library (component_cursor_sch);
				log ("generic name " & to_string (component_name), log_threshold + 2);

				-- Search in libraries for a component with this very generic name.
				-- library_cursor_sch points to the particular full library name.
				-- The libraries are searched according to their order in the library list of the module.
				-- The search is complete on the first finding of the component.
				log_indentation_up;
				log ("searching in libraries ...", log_threshold + 2);
				log_indentation_up;
				et_schematic.reset_library_cursor (library_cursor_sch);
				while library_cursor_sch /= type_full_library_names.no_element loop

					-- Set and log particular library to be searched in.
					library_name := (element (library_cursor_sch));
					log (to_string (library_name), log_threshold + 2);

					-- Get cursor of that component in library. If cursor is empty, search in
					-- next library. If cursor points to a matching component, extract ports
					-- of that component. Procedure extract_ports uses component_cursor_lib .
					component_cursor_lib := find_component (library_name, component_name);
					if component_cursor_lib = et_libraries.type_components.no_element then
						-- not found -> advance to next library (in module.libraries)
						next (library_cursor_sch); 
					else
						-- As a safety measure we make sure that the appearance of the component
						-- in the schematic equals that in the library.
						check_appearance_sch_vs_lib;
	
						extract_ports; -- uses component_cursor_lib
						-- found -> no further search required
						-- CS: write warning if component exists in other libraries ?
						exit;
					end if;
						
				end loop;

				-- IF COMPONENT NOT FOUND IN ANY LIBRARY:
				-- Early exits from the loop above leave library_cursor_sch pointing to a library.
				-- If the loop has been completed without success, library_cursor_sch points to no_element.
				-- If all libraries searched without any match -> generate error message.
				if library_cursor_sch = type_full_library_names.no_element then
					log_indentation_reset;
					log (message_error & "component with reference "  
						& et_schematic.to_string (et_schematic.component_reference (component_cursor_sch))
						& " has no generic model in any library !",
						console => true);
					raise constraint_error;
				end if;
				
				log_indentation_down;
				log_indentation_down;
				log_indentation_down;

			end if; -- no power_flag
			
			next (component_cursor_sch); -- advance to next component
		end loop;

		log_indentation_down;
-- 		log (text => "portlists complete", level => log_threshold);
		log_indentation_down;

		-- Save portlists in the module (indicated by module_cursor).
		-- Why ? The portlists are later essential for netlist generation and ERC.
		save_portlists;
		
		return portlists;
	end build_portlists;

	function first_port (component_cursor : in type_portlists.cursor) return type_ports.cursor is
	-- Returns a cursor pointing to the first port of a component in the portlists.
		port_cursor : type_ports.cursor;
	
		procedure set_cursor (
			name : in et_libraries.type_component_reference;
			ports : in type_ports.list) is
		begin
			port_cursor := first (ports);
		end set_cursor;
		begin -- first_port
		type_portlists.query_element (
			position => component_cursor,
			process => set_cursor'access);

		return port_cursor;
	end first_port;

	function port_sits_on_segment (
	-- Returns true if the given port sits on the given net segment.
		port	: in type_port'class;
		segment	: in type_net_segment'class) 
		return boolean is

		use et_geometry;
		use et_coordinates;
		use et_string_processing;
		
		sits_on_segment : boolean := false;
		d : type_distance_point_from_line;
	begin
		-- First make sure port and segment share the same module path and sheet.
		-- It is sufficient to check against the segment start coordinates.
		if same_path_and_sheet (port.coordinates, segment.coordinates_start) then
			
			-- calculate the shortes distance of point from line.
			d := distance_of_point_from_line (
				point 		=> type_2d_point (port.coordinates),
				line_start	=> type_2d_point (segment.coordinates_start),
				line_end	=> type_2d_point (segment.coordinates_end),
				line_range	=> with_end_points);

			if (not d.out_of_range) and d.distance = et_coordinates.zero_distance then
				sits_on_segment := true;
				log ("port on segment", level => 5);
			end if;

		end if;
			
		return sits_on_segment;
	end port_sits_on_segment;
		
	procedure rename_strands (
	-- Renames all strands with the name_before to the name_after.
	-- Changes the scope of the affected strands to "global".
	-- This procdure is required if a strand is connected to a power-out port.
	-- The power-out port enforces its name onto the strand.
		name_before		: in type_net_name.bounded_string;
		name_after		: in type_net_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;

		count : natural := 0;
	
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
			begin
				-- search for the strand to be renamed
				if strand.name = name_before then

					count := count + 1;
					log_indentation_up;
-- 					log (
-- 						text => to_string (strand.name) 
-- 							& " to "
-- 							& to_string (name_after) & " ...",
-- 							level => 2
-- 						);
					log (to_string (position => strand.coordinates, scope => et_coordinates.module),
						log_threshold + 1);

					log_indentation_down;

					strand.name := name_after; -- assign new name to strand
					strand.scope := global;

					renamed := true; -- signal that renaming took place
				end if;
			end do_it;
			
		begin -- rename
			while cursor /= type_strands.no_element loop
				module.strands.update_element (
					position => cursor,
					process => do_it'access);

				-- Exit prematurely if name_before was anonymous. anonymous strand names are unique.
				-- So it is ok to exit prematurely.
				if renamed and anonymous (name_before) then
					exit;
				end if;
				
				next (cursor);
			end loop;
		end rename;
		
	begin -- rename_strands
		log ("renaming strands from " & to_string (name_before)
			 & " to " & to_string (name_after) & " ...", log_threshold);
		
		rig.update_element (
			position	=> module_cursor,
			process		=> rename'access
			);

		if count > 0 then
			log ("renamed" & natural'image (count) & " strands", log_threshold);
		else
			-- CS: This should never happen
			log_indentation_reset;
			log (message_error & "strand " 
				& to_string (name_before) & " not found !");
			raise constraint_error;
		end if;
	end rename_strands;

	
	procedure update_strand_names (log_threshold : in et_string_processing.type_log_level) is
	-- Tests if a power out port is connected to a strand and renames the strand if necessary.
		use et_string_processing;
	
		portlists : type_portlists.map := type_portlists.empty_map;

		use et_coordinates;
		use et_libraries;
		
		strand		: type_strands.cursor := first_strand;
		segment		: type_net_segments.cursor;
		component	: type_portlists.cursor;
		port		: type_ports.cursor;
		
		use type_strands;
		use type_net_segments;
		use type_portlists;
		use type_ports;

		function to_net_name (port_name : in type_port_name.bounded_string) 
		-- Converts the given port name to a net name.
			return type_net_name.bounded_string is
		begin
			return type_net_name.to_bounded_string (to_string (port_name));
		end to_net_name;
		
	begin -- update_strand_names
		log (text => "updating strand names by power-out ports ...", level => log_threshold);

		-- Generate the portlists of the module indicated by module_cursor.
		portlists := build_portlists (log_threshold + 1);

		-- LOOP IN STRANDS OF MODULE
		while strand /= type_strands.no_element loop
			log_indentation_up;
			log ("strand of net " & et_schematic.to_string (element (strand).name), log_threshold + 3);

			-- LOOP IN SEGMENTS OF STRAND
			segment := first_segment (strand);
			while segment /= type_net_segments.no_element loop
				log_indentation_up;
				log ("probing segment " & to_string (element (segment)), log_threshold + 3);

				-- LOOP IN COMPONENTS (of portlists)
				component := first (portlists);
				while component /= type_portlists.no_element loop
					log_indentation_up;
					log ("probing component " & et_schematic.to_string (key (component)), log_threshold + 4);

					-- LOOP IN PORTLIST (of component)
					port := first_port (component);
					while port /= type_ports.no_element loop
						log_indentation_up;

						-- We are interested in power out ports exclusively. Only such ports may enforce their
						-- name on a strand.
						if element (port).direction = POWER_OUT then
						-- CS: skip already processed ports to improve performance

							log ("probing port " & to_string (position => element (port).coordinates), log_threshold + 4);

							-- test if port sits on segment
							if port_sits_on_segment (element (port), element (segment)) then
								log_indentation_up;
-- 								log ("match", log_threshold + 2);

								-- If strand has no name yet, it is to be named by the name of the port that sits on it.
								-- If strand has a name already, its scope must be global
								-- because power out ports are allowed in global strands exclusively !
								if et_schematic.anonymous (element (strand).name) then
									log ("component " & et_schematic.to_string (key (component)) 
										& " pin " & to_string (element (port).pin)
										& " port name " & to_string (element (port).port) 
										& " is a power output -> port name sets strand name", log_threshold + 2);

									-- rename strand
									et_schematic.rename_strands (
										name_before => element (strand).name,
										name_after => to_net_name (element (port).port),
										log_threshold => log_threshold + 3);

								elsif element (strand).scope /= global then -- strand has a name and is local or hierarchic
							
										log_indentation_reset;
										log (message_error & "component " & et_schematic.to_string (key (component)) 
											& " POWER OUT pin " & to_string (element (port).pin)
											& " port name " & to_string (element (port).port) 
											& latin_1.lf
											& "at " & to_string (element (port).coordinates, module)
											& latin_1.lf
											& "conflicts with " & to_string (element (strand).scope) 
											& " net " & et_schematic.to_string (element (strand).name) & " !");
										raise constraint_error;

								end if;
								
								log_indentation_down;
							end if;

						end if;
						
						log_indentation_down;
						next (port);
					end loop;

					log_indentation_down;
					next (component);
				end loop;


				log_indentation_down;
				next (segment);
			end loop;

			log_indentation_down;
			next (strand);
		end loop;
		
	end update_strand_names;

	
	procedure write_strands (log_threshold : in et_string_processing.type_log_level) is
	-- Writes a nice overview of strands, net segments and labels
	-- CS: output consequtive number for strands and segments (as in procedure write_nets)
		use et_string_processing;

		procedure query_label (
			segment		: in type_net_segment) is
			label_simple	: type_simple_labels.cursor	:= segment.label_list_simple.first;
			label_tag		: type_tag_labels.cursor	:= segment.label_list_tag.first;
			use type_simple_labels;
			use type_tag_labels;
		begin
			if log_level >= log_threshold + 2 then
				log_indentation_up;
				while label_simple /= type_simple_labels.no_element loop
					log ("simple label " & to_string (position => element (label_simple).coordinates));
					next (label_simple);
				end loop;

				while label_tag /= type_tag_labels.no_element loop
					log ("tag label " & to_string (position => element (label_tag).coordinates));
					next (label_tag);
				end loop;

				log_indentation_down;
			end if;
		end query_label;
	
		procedure query_segment (
			strand	: in type_strand) is
			segment : type_net_segments.cursor := strand.segments.first;
			use type_net_segments;
		begin
			if log_level >= log_threshold + 1 then
				while segment /= type_net_segments.no_element loop
					log_indentation_up;
					log ("segment " & to_string (element (segment)));

					type_net_segments.query_element (
						position	=> segment,
						process		=> query_label'access);

					log_indentation_down;				
					next (segment);
				end loop;
			end if;
		end query_segment;
	
		procedure query_strands (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
			strand : type_strands.cursor := module.strands.first;
			use type_strands;
			use et_coordinates.type_path_to_submodule;
			use ada.directories;
		begin
			if log_level >= log_threshold then
				while strand /= type_strands.no_element loop
					log_indentation_up;

					log (to_string (element (strand).name) & " scope " & to_string (element (strand).scope)
						& " in "
						& et_coordinates.to_string (et_coordinates.path (element (strand).coordinates)));
					
					type_strands.query_element (
						position	=> strand,
						process		=> query_segment'access);
					
					log_indentation_down;
					next (strand);
				end loop;
			end if;
		end query_strands;
		
	begin -- write_strands
		if log_level >= log_threshold then
			log ("strands report");
			
			type_rig.query_element (
				position	=> module_cursor,
				process		=> query_strands'access);
		end if;
	
	end write_strands;

	procedure check_strands is -- CS: currently not used
	-- Checks scope of strands across the current module (indicated by module_cursor)
	-- CS: describe the purpose of this procdure more detailled.
		use et_string_processing;

		procedure query_strands (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
			
			strand_primary		: type_strands.cursor := module.strands.first;
			strand_secondary	: type_strands.cursor;
			use type_strands;
		begin
			log ("checking local strands ...");
			
			while strand_primary /= type_strands.no_element loop
				log_indentation_up;
				
				if element (strand_primary).scope = local then

					log (
						text => to_string (element (strand_primary).name) 
							& " at " & to_string (
								position => element (strand_primary).coordinates,
								scope => et_coordinates.module),
						 level => 2);
						
					if strand_primary /= module.strands.last then
						strand_secondary := next (strand_primary);

						while strand_secondary /= type_strands.no_element loop
							if element (strand_secondary).scope = local then
								if sheet (element (strand_secondary).coordinates) /= sheet (element (strand_primary).coordinates) then
									
									if element (strand_secondary).name = element (strand_primary).name then

										put_line (standard_output, message_error & "net name conflict with net "
											& to_string (element (strand_secondary).name));

										log_indentation_reset;
										log (message_error & "net name conflict with net " 
											& to_string (element (strand_secondary).name) 
											& " at " & to_string (
												position => element (strand_secondary).coordinates,
												scope => et_coordinates.module));

										-- Explain the reason for this rule:
										log ("A local net is restricted to a single sheet ! Use global nets instead.");
								
										raise constraint_error;

									end if;
								end if;
							end if;
							
							next (strand_secondary);
						end loop;
					end if;
				end if;
					
				
				log_indentation_down;
				next (strand_primary);
			end loop;
		end query_strands;

	begin
		log ("checking strands ...");

		type_rig.query_element (
			position	=> module_cursor,
			process		=> query_strands'access);

	end check_strands;
	
	
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

	function first_net return type_nets.cursor is
	-- Returns a cursor pointing to the first net of the module (indicated by module_cursor).
		cursor : type_nets.cursor;	

		procedure set_cursor (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
 		begin
			cursor := module.nets.first;
		end set_cursor;
	
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> set_cursor'access
			);
		return cursor;
	end first_net;
	
	
	procedure link_strands (log_threshold : in et_string_processing.type_log_level) is
	-- Links local and global strands to nets (see type_module.nets).

	-- Builds the nets (see type_module.nets) of the current module from its strands (see type_module.strands).
	-- NOTE: This is NOT about generating or exporting a netlist. See package et_netlist instead.
	-- This procdure should be called AFTER netlist generation because some strands may have changed their name.
	-- (Names of strands have changee due to power-out ports connected with them.)

	-- Build the module nets. Build_nets merges the strands which are still independed of
	-- each other. For example a strand named "VCC3V3" exists on submodule A on sheet 2. 
	-- Another strand "VCC3V3" exists on submodule C on sheet 1. They do not "know" each other
	-- and must be merged into a single net.
		use et_string_processing;
		use et_schematic.type_strands;

        net_name : type_net_name.bounded_string;
	
		strand	: type_strands.cursor;
	
		procedure add_net (
		-- Creates a net with the name and the scope (local, global) of the current strand. 
		-- If strand is local, the net name is rendered to a full hierarchic name.
		-- If the net existed already, then strand is appended to the strands of the net.
			mod_name : in type_submodule_name.bounded_string;
			module   : in out type_module) is

			use et_schematic.type_nets;
			
			net_created : boolean;
			net_cursor : type_nets.cursor;

			procedure add_strand (
				name	: in type_net_name.bounded_string;
				net		: in out type_net) is
			begin
				log ("strand of net " & to_string (name), level => log_threshold + 2);
				
				if net_created then -- net has just been created
					net.scope := element (strand).scope; -- set scope of net
				end if;

				if log_level >= log_threshold + 2 then
					log_indentation_up;
					log ("strand at " & to_string (position => element (strand).coordinates, scope => et_coordinates.module));
					log_indentation_down;
				end if;
				
				-- append strand to the net
				net.strands.append (new_item => element (strand));
			end add_strand;

		begin -- add_net
			module.nets.insert (
				key 		=> net_name,
				position	=> net_cursor,
				inserted	=> net_created);

			-- If net created or already there, net_cursor points to the net where the strand is to be added.
			module.nets.update_element (
				position	=> net_cursor,
				process		=> add_strand'access);
		end add_net;

	begin -- link_strands
		log (text => "linking local and global strands to nets ...", level => log_threshold);

		log_indentation_up;

		-- loop in strands of the current module
		strand := first_strand;
		log_indentation_up;
		while strand /= type_strands.no_element loop

            case element (strand).scope is
                when local =>

					-- Output a warning if strand has no name.
					if anonymous (element (strand).name) then
						log (message_warning & "net " & to_string (element (strand).name) & " has no dedicated name !");
						-- CS: Show the lowest xy position
						log ("at " & to_string (position => element (strand).coordinates, scope => et_coordinates.module));
					end if;

					-- form the net name depending on scope
					-- For local strands the full hierarchic name of the net must be formed
					-- in order to get something like "driver.GND" :

-- 					net_name := type_net_name.to_bounded_string (
-- 						et_coordinates.to_string (et_coordinates.path (element (strand).coordinates), top_module => false)
-- 							& et_coordinates.to_string (et_coordinates.module (element (strand).coordinates))
-- 							& et_coordinates.hierarchy_separator & et_schematic.to_string (element (strand).name));

-- 					if ada.directories.base_name (to_string (top_level_schematic)) = to_string (et_coordinates.module (element (strand).coordinates)) then -- CS: make function and use it in procedure write_strands too
-- 						net_name := type_net_name.to_bounded_string (hierarchy_separator 
-- 							& et_schematic.to_string (element (strand).name));
-- 					else
-- 						net_name := type_net_name.to_bounded_string (
-- 							et_coordinates.to_string (et_coordinates.path (element (strand).coordinates))
-- 							& et_coordinates.to_string (et_coordinates.module (element (strand).coordinates))
-- 							& et_coordinates.hierarchy_separator & et_schematic.to_string (element (strand).name));
-- 					end if;

					-- if strand is in top module form a net name like "/MASTER_RESET"
					if type_path_to_submodule.is_empty (et_coordinates.path (element (strand).coordinates)) then
						net_name := type_net_name.to_bounded_string (
							hierarchy_separator
							& et_schematic.to_string (element (strand).name));

					else -- strand is in any submodule. form a net name like "/SENSOR/RESET"
						net_name := type_net_name.to_bounded_string (
							et_coordinates.to_string (et_coordinates.path (element (strand).coordinates))
							& hierarchy_separator 
							& et_schematic.to_string (element (strand).name));
					end if;
				
                    -- Create net and append strand to module.nets
                    rig.update_element (
                        position => module_cursor,
                        process => add_net'access);

				when global =>
					-- form the net name depending on scope
					net_name := element (strand).name;

                    -- Create net and append strand to module.nets
                    rig.update_element (
                        position => module_cursor,
                        process => add_net'access);

				when unknown =>
					log (message_error & "unknown scope of net !");
					raise constraint_error; -- CS: should never happen as all strands should have a scope by now

				when hierarchic =>
					null; -- CS special threatment
					
			end case;
            
			next (strand);
		end loop;
		log_indentation_down;
		log_indentation_down;
	end link_strands;

	procedure process_hierarchic_nets (log_threshold : in et_string_processing.type_log_level) is
	-- Looks up strands of hierarchic nets and appends them to the local or global nets (if connected via gui_submodules). 
	-- Hierarchic nets are mere extensions of a global or local net at deeper levels in the design hierarchy. 
	-- So every hierarchic net is connected with a local or global net at a higher level. 
	-- The link between a global or local net and a hierarchic net is the gui_submodule (see spec. of type_gui_submodule). 
	-- IMPORTANT: Gui_submodules and hierarchic nets are virtual components in a graphical GUI. Neither of them exists in reality.
		use et_string_processing;
		use type_nets;
		net : type_nets.cursor;

		-- Temparily we collect the hierarchic strands that are to be appended 
		-- (to the net being examined) here. Once the net has been examined completely
		-- we append hierarchic_strands_tmp to the strands of the net.
		hierarchic_strands_tmp : type_strands.list := type_strands.empty_list;
	
		-- This construct returned after examining a gui_submodule for a suitable hierarchic net at a deeper level:
        type type_hierachic_net is record
			available	: boolean := false; -- when false, path and port are without meaning
			path        : type_path_to_submodule.list := type_path_to_submodule.empty_list;	-- the path of the submodule
			name		: type_net_name.bounded_string := to_bounded_string (""); -- the name of the hierarchic net -- CS: rename to name
        end record;

		function on_segment (port : in type_gui_submodule_port; segment : in type_net_segment) return boolean is
		-- Returns true if given port sits on given segment.
			use et_geometry;
			distance : type_distance_point_from_line;
		begin
			distance := distance_of_point_from_line (
				point 		=> port.coordinates,
				line_start	=> type_2d_point (segment.coordinates_start),
				line_end	=> type_2d_point (segment.coordinates_end),
				line_range	=> with_end_points);

			-- start and end points of the segment are inclued in the test
			if not distance.out_of_range and distance.distance = zero_distance then
				return true;
			else
				return false;
			end if;
		end on_segment;

		function hierarchic_net (segment : in type_net_segments.cursor) return type_hierachic_net is
		-- Tests if the given segment is connected with a hierarchic net via a gui_submodule.
		-- When positive: marks the port as "processed" and returns a type_hierachic_net (see spec above):
		--	- net.available true
		--	- path to submodule where the hierarchic net is
		--	- name of the hierarchic net in the submodule

		-- One submodule after another is loaded. Then its ports are loaded one after another
		-- and tested if they are connected with the given segment.
		
			net : type_hierachic_net;
			use type_rig;

			procedure query_gui_submodules (
				mod_name	: in type_submodule_name.bounded_string;
				module 		: in out type_module) is
				submodule_cursor : type_gui_submodules.cursor := module.submodules.first; -- CS: rename to gui_submodule_cursor
				use type_gui_submodules;

				procedure query_ports (
				-- Tests if the "port" of the given gui_submodule is connected with the given net segment.
				-- If connected, the path of the gui_submodule and the submodule_name form the path to the real submodule. This
				-- path is subsequently returned. The query ends.
					submodule_name	: in type_submodule_name.bounded_string; -- The gui_submodule has a name. It is also the name of the real submodule.
					gui_submodule	: in out type_gui_submodule -- This is the gui_submodule being queried.
					) is
					-- These are the "ports" of the gui_submodule (they represent the hierarchic nets within the real submodule).
					port : type_gui_submodule_ports.cursor := gui_submodule.ports.first; -- default to first port
					use type_gui_submodule_ports;
					use type_net_segments;

					procedure mark_processed (
						name : in type_net_name.bounded_string;
						port : in out type_gui_submodule_port) is
					begin
						port.processed := true;
					end mark_processed;

					function append_submodule_to_path (
					-- This function appends the name of a submodule to a path.
					-- Required to form the full path to the submodule.
						path_in		: in type_path_to_submodule.list;
						submodule	: in type_submodule_name.bounded_string)
						return et_coordinates.type_path_to_submodule.list is
						path_out : type_path_to_submodule.list := path_in;
					begin
						type_path_to_submodule.append (
							container => path_out,
							new_item => submodule);
						return path_out;
					end append_submodule_to_path;
					
				begin -- query_ports of the given gui_submodule. Test only the non-processed ones.
					-- If "port" sits on given segment, mark the "port" as processed.
					-- NOTE: The "processed" mark prevents multiple testing of the same "port" (which could lead to a forever-loop)
					while port /= type_gui_submodule_ports.no_element loop

						-- we are interested in non-processed ports only
						if not element (port).processed then

							-- if segment is connected with port
							if on_segment (element (port), element (segment)) then

								-- mark port as processed
								update_element (
									container => gui_submodule.ports,
									position => port,
									process => mark_processed'access);

								-- form the return value
								net := (
									available	=> true, -- means: there is a subordinated hierarchical net available
										   
									-- Form the path of the real submodule (gui_submodule path + submodule name):
									path		=> append_submodule_to_path (path (gui_submodule.coordinates), submodule_name), -- example /core/LEVEL_SHIFTER

									-- The name of the subordinated hierarchical net:
									name		=> type_net_name.to_bounded_string (to_string (key (port))));

								-- prematurely exit as the return is ready now
								exit;
							end if;

						end if;

						next (port);
					end loop;
				end query_ports;

			begin -- query_gui_submodules
				-- Query gui_submodules. For each gui_submodule query its "ports".
				-- These "ports" are virtual and tell the name of the subordinated hierarchic net.
				while submodule_cursor /= type_gui_submodules.no_element loop

					update_element (
						container => module.submodules,
						position => submodule_cursor,
						process => query_ports'access);

					-- Once a hierarchic net has been found, the job is done.
					if net.available then exit; end if;
					
					next (submodule_cursor);
				end loop;
			end query_gui_submodules;
			
		begin -- hierarchic_net

			-- Locate the rig module as indicated by module_cursor. Then query the gui_submodules.
			update_element (
				container => rig,
				position => module_cursor,
				process => query_gui_submodules'access);
			
			return net;
		end hierarchic_net;


		procedure collect_hierarchic_strands (net : in type_hierachic_net) is
		-- Locates hierarchic strands as specified by given hierarchic net.
		-- "net" provides the "available" flag. If false, this procedure does nothing.
		-- "net" provides the path to the submodule to search in.
		-- "net" provides the net name to search for.

		-- IMPORTANT: As a hierarchic strand may have other subordinated hierarchic strands (via gui_submodule)
		-- the search may decend indefinitely into the hierarchy.

		-- The hierarchic strands found, are collected in the temparily collector hierarchic_strands_tmp.
		
			-- Cursor h_strand points to the hierarchic strand being examined.
			-- Defaults to the first strand of the rig module (indicated by module_cursor):
			h_strand : type_strands.cursor := first_strand;
			use type_strands;
			use type_path_to_submodule;
			use type_submodule_name;

			-- This flag goes true once the given net has been found in the submodule.
			-- It serves to warn the operator about a missing hierarchic net.
			hierarchic_net_found : boolean := false;

			procedure query_segments (
			-- Tests if the given hierarchic strand is connected to any hierarchical nets.
				h_strand	: in type_strand -- the hierachic strand being examined
				) is
				-- The cursor that points to the segment being examined.
				-- Defaults to the first segment of h_strand:
				segment: type_net_segments.cursor := h_strand.segments.first;
				use type_net_segments;

				-- If a hierarchic net is available, it will be loaded here temparily.
				h_net : type_hierachic_net;
			begin
				-- Test segment if it is connected to a hierarchic net (via gui_submodules):
				while segment /= type_net_segments.no_element loop

					-- Test if any hierarchic nets are connected (via gui_submodules):
					h_net := hierarchic_net (segment);
					-- h_net may contain a suitable hierarchic net
					
					-- Append all hierarchic strands (if any) to the net being built
					-- (see top level code of procedure process_hierarchic_nets. 
					-- The net being built is indicated by cursor "net").
					collect_hierarchic_strands (h_net);

					-- If one hierarchic net has been detected, there could be more. 
					-- This loop goes on until no more hierarchic nets are available.
					while h_net.available loop
						h_net := hierarchic_net (segment);
						collect_hierarchic_strands (h_net);
					end loop;
					
					next (segment);
				end loop;
			end query_segments;
			
		begin -- collect_hierarchic_strands

			-- If a hierarchic net is available, query all hierarchic strands of
			-- the rig module.
			-- We have a match if the path of the given hierarchic net equals the
			-- path of the h_strand AND
			-- if the name of the given hierarchic net equals the name of the h_strand.
			if net.available then
				log_indentation_up;

				log ("probing hierarchic net " & to_string (net.name) 
						& " in submodule " & to_string (net.path) & " ...",
					log_threshold + 2);
				
				while h_strand /= type_strands.no_element loop
					if element (h_strand).scope = hierarchic then
						if path (element (h_strand).coordinates) = net.path then
							if element (h_strand).name = net.name then
								hierarchic_net_found := true;

								log ("reaches down into submodule " 
									& to_string (net.path) 
									& " as net " & to_string (net.name),
									log_threshold + 1
									);

								log_indentation_up;
								log ("strand " & to_string (lowest_xy (element (h_strand), log_threshold + 3)),
									 log_threshold + 2
									);
								log_indentation_down;

								-- append the strand to the temparily collection of hierarchic strands
								type_strands.append (
									container => hierarchic_strands_tmp,
									new_item => element (h_strand));

								-- Test if hierarchic h_strand itself is connected to any gui_submodules.
								-- So we query the segments of h_strand for any hierarchic strands connected.
								query_element (
									position => h_strand,
									process => query_segments'access);

							end if;
						end if;
					end if;
					next (h_strand);
				end loop;

				-- Raise warning if hierarchic net not found in submodule:
				if not hierarchic_net_found then
					log (message_warning & "hierarchic net " & to_string (net.name) 
						& " in submodule " & to_string (net.path) 
						& " not found ! "
						& "Hierarchic sheet in parent module requires this net !");
				end if;
				
				log_indentation_down;
			end if;
		end collect_hierarchic_strands;
		
		procedure query_strands (
		-- Looks for any hierarchic nets connected via gui_submodules with the given net.
			net_name : in type_net_name.bounded_string; -- the name of the net being examined
			net      : in type_net -- the net being examined
			) is
			use type_strands;
			-- The cursor pointing to the strand of the net. Defaults to the first strand.
			strand : type_strands.cursor := net.strands.first; 

			procedure query_segments (
			-- Looks for any hierarchic nets connected via gui_submodules with the given net.
				strand   : in type_strand -- the strand being examined
				) is 
				-- The cursor pointing to the segment of the strand. Defaults to the first segment.
				use type_net_segments;
				segment  : type_net_segments.cursor := strand.segments.first;

				-- If a hierarchic net is available, it will be loaded here temparily.
				h_net : type_hierachic_net;
			begin
				-- Load one segment after another and test if the segment
				-- is connected with any hierarchic nets (at deeper levels in the design hierarchy).
				while segment /= type_net_segments.no_element loop

					-- Test if any hierarchic nets are connected (via gui_submodules):
					h_net := hierarchic_net (segment);
					-- h_net may contain a suitable hierarchic net
					
					-- Append all hierarchic strands (if any) to the net being built
					-- (see top level code of procedure process_hierarchic_nets. 
					-- The net being built is indicated by cursor "net").
					collect_hierarchic_strands (h_net);

					-- If one hierarchic net has been detected, there could be more. 
					-- This loop goes on until no more hierarchic nets are available.
					while h_net.available loop
						h_net := hierarchic_net (segment);
						collect_hierarchic_strands (h_net);
					end loop;
					
					next (segment);
				end loop;
			end query_segments;

		begin -- query_strands
			-- Load one strand after another. Then query its segments.
			while strand /= type_strands.no_element loop
				query_element (
					position => strand,
					process => query_segments'access);

				next (strand);
			end loop;
		end query_strands;

		procedure append_hierarchic_strands (
			--net_name : in type_net_name.bounded_string;
			net_cursor : in type_nets.cursor;
			strands	 : in type_strands.list
			) is
			use type_rig;

			procedure locate_net (
				module_name	: in type_submodule_name.bounded_string;
				module		: in out type_module
				) is
				use type_nets;
				--net_cursor : type_nets.cursor;

				procedure append_strands (
					net_name	: in type_net_name.bounded_string;
					net			: in out type_net
					) is
					use type_strands;
				begin
					splice (
						target => net.strands,
						before => type_strands.no_element,
						source => hierarchic_strands_tmp);
				end append_strands;

			begin -- locate_net
				type_nets.update_element (
					container => module.nets,
					position => net_cursor,
					process => append_strands'access);
				
			end locate_net;
			
		begin -- append_hierarchic_strands
			-- locate module as indicated by module_cursor
			type_rig.update_element (
				container => rig,
				position => module_cursor,
				process => locate_net'access);
		end append_hierarchic_strands;
			
    begin -- process_hierarchic_nets
		log (text => "linking hierarchic strands to nets ...", level => log_threshold);

		-- Load one net after another. 
		-- NOTE: The nets of the module are either local or global (see spec type_net_scope).
		-- Then query the strands of the net.
		net := first_net;
		log_indentation_up;
		while net /= type_nets.no_element loop
			log ("net " & to_string (key (net)), log_threshold + 1);

			-- Examine the global or local net for any hierarchical nets connected to it.
			-- If there are any, they are collected in hierarchic_strands_tmp.
			query_element (
				position => net,
				process => query_strands'access);

			-- What we have collected in hierarchic_strands_tmp is now appended to the net.
			append_hierarchic_strands (
				--net_name => key (net),
				net_cursor => net,
				strands => hierarchic_strands_tmp); 
				-- NOTE: clears hierarchic_strands_tmp by its own
				-- in order to provide a clean collector for the next net.

			next (net);
		end loop;
		log_indentation_down;

	end process_hierarchic_nets;
	

	procedure write_nets (log_threshold : in et_string_processing.type_log_level) is
	-- Writes a nice overview of all nets, strands, segments and labels.
	-- Bases on the element "nets" of the modules. See specification of type_module.
		use et_string_processing;
	
		procedure query_label (
			segment : in type_net_segment) is
			label_simple	: type_simple_labels.cursor	:= segment.label_list_simple.first;
			label_tag		: type_tag_labels.cursor	:= segment.label_list_tag.first;
			use type_simple_labels;
			use type_tag_labels;
		begin
			if log_level >= log_threshold + 3 then
				
				log_indentation_up;
				while label_simple /= type_simple_labels.no_element loop
					log ("simple label at " & to_string (position => element (label_simple).coordinates, scope => xy));
					next (label_simple);
				end loop;

				while label_tag /= type_tag_labels.no_element loop
					if element (label_tag).hierarchic then
						log ("hierarchic label at " 
							& to_string (position => element (label_tag).coordinates, scope => xy));
					end if;

					if element (label_tag).global then
						log ("global label at " 
							& to_string (position => element (label_tag).coordinates, scope => xy));
					end if;
					
					next (label_tag);
				end loop;
				
				log_indentation_down;
			end if;
		end query_label;
		
		procedure query_segment (
			strand : in type_strand) is
			segment : type_net_segments.cursor := strand.segments.first;
			use type_net_segments;

			-- for the segment we provide a consequtive number which has no further meaning
			segment_number : count_type := 1;			
		begin
			if log_level >= log_threshold + 2 then
				log_indentation_up;
				while segment /= type_net_segments.no_element loop
					log ("segment #" 
						& count_type'image (segment_number) 
						& latin_1.space
						& to_string (segment => element (segment), scope => xy));

					query_element (
						position => segment,
						process => query_label'access);
					
					segment_number := segment_number + 1;
					next (segment);
				end loop;
				log_indentation_down;
			end if;
		end query_segment;
		
		procedure query_strand (
			net_name : in type_net_name.bounded_string;
			net : in type_net) is
			
			strand : type_strands.cursor := net.strands.first;
			use type_strands;

			-- for the strand we provide a consequtive number which has no further meaning
			strand_number : count_type := 1;			
		begin -- query_strand
			if log_level >= log_threshold + 1 then
				log_indentation_up;
				while strand /= type_strands.no_element loop
					log ("strand #" & trim (count_type'image (strand_number), left) &
						" at " & to_string (position => element (strand).coordinates, scope => et_coordinates.module)
						);

					query_element (
						position => strand,
						process => query_segment'access);
					
					strand_number := strand_number + 1;
					next (strand);
				end loop;
				log_indentation_down;
			end if;
		end query_strand;
		
		procedure query_net (
			mod_name : in type_submodule_name.bounded_string;
			module : in type_module) is
			net : type_nets.cursor := module.nets.first;
			use type_nets;
		begin
			log_indentation_up;
			while net /= type_nets.no_element loop
				log ("net " & to_string (key (net)));

				query_element (
					position => net,
					process => query_strand'access);
				
				next (net);
			end loop;

			log_indentation_down;
		end query_net;

		use type_rig;
		
	begin -- write_nets
		if log_level >= log_threshold then
			log ("net report");
			log_indentation_up;
				
			first_module;
			while module_cursor /= type_rig.no_element loop
					
				log ("module " & to_string (key (module_cursor)));

				query_element (
					position => module_cursor,
					process => query_net'access);
				
				next (module_cursor);
			end loop;

			log_indentation_down;
		end if;
	end write_nets;

		
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
		reference		: in et_libraries.type_component_reference;
		component		: in type_component;
		log_threshold 	: in et_string_processing.type_log_level) is
		
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
				write_component_properties (component => cursor, log_threshold => log_threshold + 1);
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
	
	procedure add_unit (
	-- Adds a unit into the given commponent.
		reference		: in et_libraries.type_component_reference;
		unit_name		: in et_libraries.type_unit_name.bounded_string;
		unit 			: in type_unit;
		log_threshold	: in et_string_processing.type_log_level) is

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
-- 				if log_level >= 2 then				
					write_unit_properties (unit => cursor, log_threshold => log_threshold);
-- 				end if;
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
	-- Resets the given component cursor to the begin of the component list
	-- of the module indicated by module_cursor.
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

	function compare_ports (left, right : in type_port_with_reference) return boolean is
	-- Returns true if left comes before right. Compares by component name and pin name.
	-- If left equals right, the return is false.	
	-- CS: needs verification !
		result : boolean := false;
		use et_libraries;
		use et_schematic;
	begin
		-- First we compare the component reference.
		-- Examples: C56 comes before R4, LED5 comes before LED7
		if compare_reference (left.reference, right.reference) then
			result := true;

		-- If equal references, compare pin names
		elsif type_pin_name.">" (left.pin, right.pin) then
			result := true;

		-- If equal pin names, compare port names -- CS: should never happen. raise alarm ?
		elsif type_port_name.">" (left.port, right.port) then
			result := true;
			
		else
			result := false;
		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end compare_ports;
	
	function net_count return count_type is
	-- Returns the number of nets of the current module as string.
		count : count_type := 0;
	
		procedure count_nets (
			module_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
		begin
			count := length (module.netlist);
		end count_nets;

	begin -- net_count
		type_rig.query_element (
			position => module_cursor,
			process => count_nets'access);

		return count;
	end net_count;
	
	procedure make_netlists (log_threshold : in et_string_processing.type_log_level) is
	-- Builds the netlists of all modules of the rig.
	-- Addresses ALL components both virtual and real. Virtual components are things like GND or VCC symbols.
	-- Virtual components are filtered out on exporting the netlist in a file.
	-- Bases on the portlists and nets/strands information of the module.
	-- Netlists are exported in individual project directories in the work directory of ET.
	-- These project directories have the same name as the module indicated by module_cursor.
	
		use et_string_processing;
		use type_rig;

		function make_netlist return type_netlist.map is
		-- Generates the netlist of the current module (indicated by module_cursor).
		-- module.portlists provide the port coordinates. 
		-- module.nets provides the strands and nets.
		-- With this information we make the netlist of the current module.
		
			-- the netlist being built. it is returnd to the calling unit.
			netlist : type_netlist.map;

			procedure query_nets (
			-- Tests if a net of the given module is connected to any component port.
			-- Creates a net in the netlist (type_module.netlist) with the same name 
			-- as the net being examined (type_module.nets).
			-- Component ports connected with the net are collected in portlist of the 
			-- net being built (see procedure add_port below).
				module_name	: in type_submodule_name.bounded_string;
				module		: in type_module) is

				use type_nets;
				net_cursor 		: type_nets.cursor := module.nets.first; -- points to the net being read
				net_in_netlist	: type_netlist.cursor; -- points to the net being built in the netlist
				net_created		: boolean := false; -- goes true once the net has been created in the netlist
				
				procedure query_strands (
				-- Tests if a strand of the given net is connected to any component port.
					net_name	: in type_net_name.bounded_string;
					net			: in type_net) is
					use type_strands;
					strand_cursor : type_strands.cursor := net.strands.first; -- points to the first strand of the net

					procedure query_segments (strand : in type_strand) is
					-- Tests the net segments of the given strand if they are connected with any component ports.
					-- For every segment, all component ports must be tested.
						use type_net_segments;
						segment		: type_net_segments.cursor := strand.segments.first; -- points to the segment being read
						use type_portlists;
						component_cursor : type_portlists.cursor; -- points to the component being read

						procedure query_ports (
						-- Tests the ports of the given component if they sit on the current net segment.
							component	: in type_component_reference;
							ports		: in type_ports.list) is
							use type_ports;
							port_cursor : type_ports.cursor := ports.first; -- points to the first port of the component

							procedure mark_port_as_processed is
							-- mark port in portlist as processed
							
								procedure locate_component (
								-- Locates the component within the portlist of the submodule
									module_name	: in type_submodule_name.bounded_string;
									module 		: in out type_module) is
	
									procedure locate_port (
									-- Locates the port of the component
										component	: in type_component_reference;
										ports		: in out type_ports.list) is

										procedure mark_it (port : in out type_port) is
										begin
											port.processed := true;
										end mark_it;
											
									begin -- locate_port
										update_element (
											container => ports,
											position => port_cursor,
											process => mark_it'access);
									end locate_port;
										
								begin -- locate_component 
									type_portlists.update_element (
										container	=> module.portlists,
										position	=> component_cursor,
										process 	=> locate_port'access);
								end locate_component;
									
							begin -- mark_port_as_processed
								-- locate the submodule in the rig
								update_element (
									container => rig,
									position => module_cursor,
									process => locate_component'access);
							end mark_port_as_processed;
							
							procedure add_port (
							-- Adds the port (indicated by cursor "port" to the portlist of the net being built.
								net_name	: in type_net_name.bounded_string;
								ports		: in out type_ports_with_reference.set) is
								inserted : boolean;
								cursor : type_ports_with_reference.cursor;
							begin -- add_port
								-- If a port sits on the point where two segments meet, the same port should be inserted only once.
								-- Thus we have the obligatory flag "inserted". 
								type_ports_with_reference.insert (
									container	=> ports,
									position	=> cursor,
									inserted	=> inserted,
									-- We add the port and extend it with the component reference.
									new_item	=> (element (port_cursor) with component));

								if not inserted then -- port already in net
									log_indentation_up;
									log ("already processed -> skipped", log_threshold + 3);
									log_indentation_down;
								end if;
							end add_port;

						begin -- query_ports
							while port_cursor /= type_ports.no_element loop

								-- Probe only those ports (in the portlists) which are in the same 
								-- path and at the same sheet as the port.
								-- Probing other ports would be a waste of time.
								if et_coordinates.same_path_and_sheet (
									left => strand.coordinates, 
									right => element (port_cursor).coordinates ) then

									if not element (port_cursor).processed then
								
										log_indentation_up;
										log ("probing " & to_string (component) 
												& " pin/pad " & to_string (element (port_cursor).pin)
												& latin_1.space
												& to_string (position => element (port_cursor).coordinates, scope => et_coordinates.module),
											log_threshold + 5);

										-- test if port sits on segment
										if port_sits_on_segment (element (port_cursor), element (segment)) then
											log_indentation_up;
										
											log ("connected with " & to_string (component) 
													& " pin/pad " & to_string (element (port_cursor).pin)
													& latin_1.space
													& to_string (position => element (port_cursor).coordinates, scope => et_coordinates.module),
												log_threshold + 3);
											
											log_indentation_down;

											-- add port to the net being built
											type_netlist.update_element (
												container => netlist,
												position => net_in_netlist,
												process => add_port'access);

											-- Mark the port (in the portlists) as processed.
											-- Why ? A port can be connected to ONLY ONE net. So once it is
											-- detected here, it would be a wast of computing time to 
											-- test if the port is connected to other nets.
											mark_port_as_processed;
										end if;
											
										log_indentation_down;
									end if;
								end if;

								next (port_cursor);
							end loop;
						end query_ports;
						
					begin -- query_segments
						log_indentation_up;
					
						while segment /= type_net_segments.no_element loop

							log ("segment " & to_string (element (segment)), log_threshold + 4);

							-- reset the component cursor, then loop in the component list 
							component_cursor := module.portlists.first;	-- points to the component being read
							while component_cursor /= type_portlists.no_element loop

								-- query the ports of the component
								type_portlists.query_element (
									position => component_cursor,
									process => query_ports'access);

								next (component_cursor);
							end loop;
							
							next (segment);
						end loop;
							
						log_indentation_down;	
					end query_segments;

				begin -- query_strands
					log_indentation_up;
				
					while strand_cursor /= type_strands.no_element loop

						-- log strand coordinates
						log ("strand " & to_string (element (strand_cursor).coordinates, scope => et_coordinates.module),
							 log_threshold + 3);

						query_element (
							position => strand_cursor,
							process => query_segments'access);
				
						next (strand_cursor);
					end loop;
						
					log_indentation_down;
				end query_strands;

			begin -- query_nets
				log_indentation_up;
			
				while net_cursor /= type_nets.no_element loop

					-- log the name of the net being built
					log (to_string (key (net_cursor)), log_threshold + 2);
				
					-- create net in netlist
					insert (
						container => netlist,
						key => key (net_cursor),
						new_item => type_ports_with_reference.empty_set,
						position => net_in_netlist,
						inserted => net_created);

					-- CS: evaluate flag net_created ?

					-- search for ports connected with the net being built
					query_element (
						position => net_cursor,
						process => query_strands'access);

					next (net_cursor);
				end loop;

				log_indentation_down;	
			end query_nets;
					
		begin -- make_netlist (NOTE: singluar !)
			query_element (
				position => module_cursor,
				process => query_nets'access);

			return netlist;
		end make_netlist;

		procedure add_netlist (
			module_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is
		begin
			module.netlist := make_netlist;
		end add_netlist;

	begin -- make_netlists (note plural !)
		log (text => "building rig netlists ...", level => log_threshold);
		log_indentation_up;
		
		-- We start with the first module of the rig.
		first_module;

		-- Process one rig module after another.
		-- module_cursor points to the module in the rig.
		while module_cursor /= type_rig.no_element loop
			log ("module " & to_string (key (module_cursor)), log_threshold + 1);

			update_element (
				container => rig,
				position => module_cursor,
				process => add_netlist'access);

			log (" net count total" & count_type'image (net_count), log_threshold + 1);
			next (module_cursor);
		end loop;

		log_indentation_down;
	end make_netlists;
	
	procedure export_netlists (log_threshold : in et_string_processing.type_log_level) is
	-- Exports/Writes the netlists of the rig in separate files.
	-- Addresses real components exclusively. Virtual things like GND symbols are not exported.
	-- Call this procedure after executing procedure make_netlist !
		use type_rig;
		use ada.directories;
		use et_general;
		use et_string_processing;
		use et_export;
		
		netlist_handle : ada.text_io.file_type;
		netlist_file_name : type_netlist_file_name.bounded_string;
	
		procedure query_nets (
			module_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
			net_cursor	: type_netlist.cursor := module.netlist.first;

			procedure query_ports (
				net_name	: in type_net_name.bounded_string;
				ports		: in type_ports_with_reference.set) is
				port_cursor : type_ports_with_reference.cursor := ports.first;
			begin
				log_indentation_up;
				--log ("ports" & count_type'image (length (ports)), log_threshold + 3);

				while port_cursor /= type_ports_with_reference.no_element loop

					-- we export only ports of real components
					if element (port_cursor).appearance = sch_pcb then

						-- write reference, port, pin in netlist (all in a single line)
						log ( 
							to_string (element (port_cursor).reference) & latin_1.space
							& to_string (element (port_cursor).port) & latin_1.space
							& to_string (element (port_cursor).pin)
							& to_string (element (port_cursor).direction),
							log_threshold + 3);
					
						-- write reference, port, pin in netlist (all in a single line)
						put_line (netlist_handle, 
							to_string (element (port_cursor).reference) & latin_1.space
							& to_string (element (port_cursor).port) & latin_1.space
							& to_string (element (port_cursor).pin)
							& to_string (element (port_cursor).direction, preamble => false));

					end if;
						
					next (port_cursor);
				end loop;

				log_indentation_down;
			end query_ports;
			
		begin -- query_nets
			log_indentation_up;

			-- output the net names. then query the ports/pins of the net
			while net_cursor /= type_netlist.no_element loop

				-- log and write net name in netlist
				log (to_string (key (net_cursor)), log_threshold + 2);
				new_line (netlist_handle);
				put_line (netlist_handle, to_string (key (net_cursor)));

				-- query ports of net
				type_netlist.query_element (
					position	=> net_cursor,
					process		=> query_ports'access);
				
				next (net_cursor);
			end loop;
				
			log_indentation_down;	
		end query_nets;

	begin -- export_netlists

		-- We start with the first module of the rig.
		first_module;

		log ("exporting rig netlists ...", log_threshold);
		log_indentation_up;
		
		while module_cursor /= type_rig.no_element loop
			log ("module " & to_string (key (module_cursor)), log_threshold + 1);

			log_indentation_up;
			
			-- compose the netlist file name and its path like "../ET/motor_driver/CAM/motor_driver.net"
			create_project_directory (to_string (key (module_cursor)), log_threshold + 2);
			netlist_file_name := type_netlist_file_name.to_bounded_string 
				(
				compose (
					containing_directory => compose 
						(
						containing_directory => compose (work_directory, to_string (key (module_cursor))),
						name => et_export.directory_cam
						),
					name => to_string (key (module_cursor)),
					extension => extension_netlist)
				);

			-- create the netlist (which inevitably and intentionally overwrites the previous file)
			log ("creating netlist file " & type_netlist_file_name.to_string (netlist_file_name), log_threshold + 2);
			create (
				file => netlist_handle,
				mode => out_file, 
				name => type_netlist_file_name.to_string (netlist_file_name));

			put_line (netlist_handle, comment_mark & " " & system_name & " netlist");
			put_line (netlist_handle, comment_mark & " date " & string (date_now));
			put_line (netlist_handle, comment_mark & " module " & to_string (key (module_cursor)));
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " net count total" & count_type'image (net_count));
			-- CS: statistics about pin count ?
			
			put_line (netlist_handle, comment_mark);
			put_line (netlist_handle, comment_mark & " legend:");
			put_line (netlist_handle, comment_mark & "  net name");
			put_line (netlist_handle, comment_mark & "  component port pin/pad direction");
			put_line (netlist_handle, comment_mark & " " & row_separator_single);

			-- do the export
			query_element (
				position	=> module_cursor,
				process		=> query_nets'access);

			new_line (netlist_handle);
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " end of list");
			
			close (netlist_handle);
			log_indentation_down;
			
			next (module_cursor);
			
		end loop;
			
		log_indentation_down;
		
	end export_netlists;

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
		
		begin -- make_bom
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

	function to_string (category : in type_component_category) return string is
	-- Returns the given category as string.
	begin
		return to_lower (type_component_category'image (category));
	end to_string;
	
	function make_statistics_components_and_ports (log_threshold : in et_string_processing.type_log_level)
		return type_statistics_components_and_ports is
	-- Returns statistics about components and ports on the module indicated by module_cursor.
	-- The numbers are extracted from the components and portlists of the module exclusively.
		statistics : type_statistics_components_and_ports;
	
		arrow : constant string (1..4) := " -> ";

		use et_string_processing;		
		
		procedure count_components (
			name	: in type_submodule_name.bounded_string;
			module	: in type_module) is
		
			component : type_components.cursor := module.components.first;
			use type_components;
			begin -- count_components
			statistics.components_total := module.components.length;

			while component /= type_components.no_element loop

				case element (component).appearance is
					when sch => -- virtual
						log (to_string (key (component)) & " -> " & to_string (virtual), log_threshold);
						statistics.components_virtual := statistics.components_virtual + 1;

					when sch_pcb => -- real
						statistics.components_real := statistics.components_real + 1;
					
						if element (component).bom = YES then
							statistics.components_mounted := statistics.components_mounted + 1;
						
							log (to_string (key (component))
								& arrow & to_string (real), log_threshold);

						else
							log (to_string (key (component)) 
								& arrow & to_string (real) 
								& arrow & not_mounted, log_threshold);

						end if;
							
				end case;

				next (component);
			end loop;
				
		end count_components;


		procedure count_ports (
			name	: in type_submodule_name.bounded_string;
			module	: in type_module) is

			use type_portlists;
			portlist : type_portlists.cursor := module.portlists.first;

			procedure count (
				component	: in type_component_reference;
				ports		: in type_ports.list) is
				port : type_ports.cursor := ports.first;
			begin
				while port /= type_ports.no_element loop
			
					if element (port).processed then
						statistics.ports_total := statistics.ports_total + 1;
					
						-- CS: log port
					end if;

					next (port);
				end loop;
			end count;
				
		begin -- count_ports
			while portlist /= type_portlists.no_element loop

				query_element (
					position	=> portlist,
					process		=> count'access);
				
				next (portlist);
			end loop;
				
		end count_ports;

	begin -- make_statistics_components_and_ports

		type_rig.query_element (
			position	=> module_cursor,
			process		=> count_components'access
			);

		type_rig.query_element (
			position	=> module_cursor,
			process		=> count_ports'access
			);
			
		
		return statistics;
	end make_statistics_components_and_ports;

	function components_statistics (
		statistics_components_and_ports : in type_statistics_components_and_ports;
		category : in type_component_category) return string is
	-- Returns the number of components as string. Category determines the kind of 
	-- components to address.
	begin
		case category is
			when total =>
				return count_type'image (
					statistics_components_and_ports.components_virtual 
					+ statistics_components_and_ports.components_real);

			when virtual =>
				return count_type'image (statistics_components_and_ports.components_virtual);

			when real =>
				return count_type'image (statistics_components_and_ports.components_real);

			when mounted =>
				return count_type'image (statistics_components_and_ports.components_mounted);

		end case;
	end components_statistics;

	
	procedure write_statistics (log_threshold : in et_string_processing.type_log_level) is
	-- Generates the statistics on components and nets of the rig.
	-- Breaks statistics up into submodules, general statistics (CAD) and CAM related things.
		statistics_file_name_cad: type_statistic_file_name.bounded_string;
		statistics_file_name_cam: type_statistic_file_name.bounded_string;
		statistics_handle_cad	: ada.text_io.file_type;
		statistics_handle_cam	: ada.text_io.file_type;

		component : type_components.cursor;
		-- CS net

		components_and_ports : type_statistics_components_and_ports;
		
		use ada.directories;
		use et_general;
		use type_components;
		use type_rig;
		use et_string_processing;
		use et_netlist;
	begin -- write_statistics
		first_module;
		
		log ("writing statistics ...", log_threshold);
		log_indentation_up;
		
		while module_cursor /= type_rig.no_element loop

			log ("module " & to_string (key (module_cursor)), log_threshold + 1);
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
			log ("CAD statistics file " & type_statistic_file_name.to_string (statistics_file_name_cad), log_threshold + 2);
			create (
				file => statistics_handle_cad,
				mode => out_file, 
				name => type_statistic_file_name.to_string (statistics_file_name_cad));

			log_indentation_up;
			put_line (statistics_handle_cad, comment_mark & " " & system_name & " CAD statistics");
			put_line (statistics_handle_cad, comment_mark & " date " & string (date_now));
			put_line (statistics_handle_cad, comment_mark & " module " & to_string (key (module_cursor)));
			put_line (statistics_handle_cad, comment_mark & " " & row_separator_double);

			components_and_ports := make_statistics_components_and_ports (log_threshold + 1);
			
			-- components
			put_line (statistics_handle_cad, "components");
			put_line (statistics_handle_cad, " total   " & components_statistics (components_and_ports, total));
			put_line (statistics_handle_cad, " real    " & components_statistics (components_and_ports, real));
			put_line (statistics_handle_cad, latin_1.space & et_string_processing.mounted & latin_1.space 
				& components_statistics (components_and_ports, mounted));
			put_line (statistics_handle_cad, " virtual " & components_statistics (components_and_ports, virtual));
-- 			-- CS: resitors, leds, transitors, ...
-- 
-- 			-- nets
-- 			put_line (statistics_handle_cad, "nets");
-- 			put_line (statistics_handle_cad, " total  " & count_type'image (net_count));
-- 
-- 			-- As for the total number of ports, we take all ports into account (inc. virtual ports of virtual components like GND symbols).
-- 			--put_line (statistics_handle_cad, "  ports " & count_type'image (port_count));
-- 			
-- 			-- finish statistics			
-- 			put_line (statistics_handle_cad, comment_mark & " " & row_separator_single);
-- 			put_line (statistics_handle_cad, comment_mark & " end of list");
-- 			log_indentation_down;
			close (statistics_handle_cad);


-- 
-- 			-- CAM
-- 			-- compose the CAM statistics file name and its path like "../ET/motor_driver/CAM/motor_driver.stat"
-- 			statistics_file_name_cam := type_statistic_file_name.to_bounded_string 
-- 				(
-- 				compose (
-- 					containing_directory => compose 
-- 						(
-- 						containing_directory => compose (work_directory, to_string (key (module_cursor))),
-- 						name => et_export.directory_cam
-- 						),
-- 					name => to_string (key (module_cursor)),
-- 					extension => extension_statistics)
-- 				);
-- 
-- 			-- create the statistics file (which inevitably and intentionally overwrites the previous file)
-- 			log (text => "CAM statistics file " & type_statistic_file_name.to_string (statistics_file_name_cam), level => 1);
-- 			create (
-- 				file => statistics_handle_cam,
-- 				mode => out_file, 
-- 				name => type_statistic_file_name.to_string (statistics_file_name_cam));
-- 
-- 			log_indentation_up;
-- 			put_line (statistics_handle_cam, comment_mark & " " & system_name & " CAM statistics");
-- 			put_line (statistics_handle_cam, comment_mark & " date " & string (date_now));
-- 			put_line (statistics_handle_cam, comment_mark & " module " & to_string (key (module_cursor)));
-- 			put_line (statistics_handle_cam, comment_mark & " " & row_separator_double);
-- 
-- 			-- components
-- 			put_line (statistics_handle_cam, "components");
-- 			put_line (statistics_handle_cam, " total" & count_type'image (components_real (mounted_only => true)));
-- 			-- As for the total number of real component ports, we take all ports into account for which a physical
-- 			-- pad must be manufactured. Here it does not matter if a component is to be mounted or not, if a pin is connected or not.
-- 			-- CS: THT/SMD
-- 			-- CS: THT/SMD/pins/pads
-- 			-- CS: resitors, leds, transitors, ...
-- 			new_line (statistics_handle_cam);
-- 
-- 			-- nets
-- 			put_line (statistics_handle_cam, "nets");
-- 			-- CS: currently we get the net and pin numbers from et_netlist.rig.
-- 			-- In the future this data should be taken from et_schematic.rig.
-- 			et_netlist.set_module (key (module_cursor));
-- 			put_line (statistics_handle_cam, " total" & count_type'image (et_netlist.net_count));
-- 
-- 			-- finish statistics
-- 			put_line (statistics_handle_cam, comment_mark & " " & row_separator_single);
-- 			put_line (statistics_handle_cam, comment_mark & " end of list");
-- 			log_indentation_down;
-- 			close (statistics_handle_cam);
-- 
-- 
-- 
-- 			
			log_indentation_down;
			next (module_cursor);
		end loop;
			
		
	end write_statistics;

	
end et_schematic;
-- Soli Deo Gloria

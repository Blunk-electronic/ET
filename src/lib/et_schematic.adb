------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET SCHEMATIC                              --
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
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;
with et_coordinates;
with et_string_processing;
with et_geometry;
with et_export;
with et_import;
with et_configuration;
with et_csv;
with et_kicad;

package body et_schematic is

	function to_net_name (net_name : in string) return type_net_name.bounded_string is
	-- Converts a string to a type_net_name.
	begin
		return type_net_name.to_bounded_string (net_name);
	end to_net_name;
	
	function to_string (net_name : in type_net_name.bounded_string) return string is
	-- Returns the given net name as string.
	begin
		return type_net_name.to_string (net_name);
	end to_string;

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

	function to_net_label_text_size (text : in string) return type_net_label_text_size is
	-- Converts a string to type_net_label_text_size.
	begin
		return type_net_label_text_size'value (text);
	end to_net_label_text_size;

	
-- 	procedure write_label_properties (label : in type_net_label) is
-- 	-- Writes the properties of the given net label in the logfile.
-- 		use et_string_processing;
-- 		use et_coordinates;
-- 
-- 		log_threshold : type_log_level := 2;
-- 	begin
-- 		log_indentation_up;
-- 		
-- 		case label.label_appearance is
-- 			when simple =>
-- 				log (text => "simple label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
-- 				
-- 			when tag =>
-- 				if label.hierarchic then
-- 					log (text => "hierarchic label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
-- 				end if;
-- 				if label.global then
-- 					log (text => "global label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
-- 				end if;
-- 					-- CS: directon, global, hierarchic, style, ...
-- 		end case;
-- 
-- 		log_indentation_up;
-- 		log (text => to_string (label.orientation), level => log_threshold + 1);
-- 		
-- 		case label.label_appearance is
-- 			when simple =>
-- 				null;
-- 			when tag =>
-- 				null;
-- 				--put("tag label ");
-- 				-- CS: directon, global, hierarchic, style, ...
-- 		end case;
-- 
-- 		log_indentation_down;
-- 		log_indentation_down;
-- 
-- 	end write_label_properties;
-- 
-- 	function to_string (label : in type_net_label; scope : in type_scope) return string is
-- 	-- Returns the coordinates of the given label as string.
-- 	begin
-- 		return (to_string (position => label.coordinates, scope => scope));
-- 	end to_string;
	
	function to_string (junction : in type_net_junction; scope : in type_scope) return string is
	-- Returns the position of the given junction as string.
	begin	
		return (to_string (position => junction.coordinates, scope => scope));
	end to_string;

	procedure write_note_properties (
		note 			: in et_schematic.type_note;
		log_threshold	: in et_string_processing.type_log_level := 0) is
	-- Writes the properties of the given note
		use et_string_processing;
		use et_coordinates;
	begin
		log ("text note" & to_string (position => note.coordinates, scope => xy), log_threshold);

		log_indentation_up;

		-- content
		if et_libraries.type_text_content.length (note.content) > 0 then
			log (text => "content '" & type_text_content.to_string (note.content) & "'", level => log_threshold);
		else
			log (text => et_string_processing.message_warning & "no content !", level => log_threshold); 
		end if;
	
		if log_level >= log_threshold + 1 then
			
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
	end write_note_properties;

	

	

	function to_package_name (
		library_name	: in type_full_library_name.bounded_string; -- ../libraries/transistors.lib
		generic_name	: in et_libraries.type_component_generic_name.bounded_string; -- TRANSISTOR_PNP
		package_variant	: in type_component_variant_name.bounded_string) -- N, D
		return type_component_package_name.bounded_string is
	-- Returns the package name for of the given component.
	-- CS move to et_kicad ?
		package_name : type_component_package_name.bounded_string; -- to be returned
	begin -- to_package_name
		-- CS
		return package_name;
	end to_package_name;
	

	procedure check_net_name_length (net : in string) is
	-- Tests if the given net name is longer than allowed.	
		use et_string_processing;
	begin
		if net'length > net_name_length_max then
			log_indentation_reset;
			log (message_error & "max. number of characters for net name is" 
				 & positive'image (net_name_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_net_name_length;

	procedure check_net_name_characters (
		net			: in type_net_name.bounded_string;
		characters	: in character_set := net_name_characters) is
	-- Tests if the given net name contains only valid characters as specified
	-- by given character set.
		use et_string_processing;
		invalid_character_position : natural := 0;
		inversion_mark_position : natural := 0;
	begin
		-- Test given net name and get position of possible invalid characters.
		invalid_character_position := index (
			source => net,
			set => characters,
			test => outside);

		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "invalid character in net name '" 
				 & to_string (net) & "' at position" 
				 & natural'image (invalid_character_position) & " !",
				 console => true);

			-- CS: show allowed characters
			raise constraint_error;
		end if;

		-- If there is an inversion mark, it must be at the very end of the net name.
		inversion_mark_position := type_net_name.index (net, net_inversion_mark);
		if inversion_mark_position > 0 then
			if inversion_mark_position /= type_net_name.length (net) then
				log_indentation_reset;
				log (message_error & "net " & to_string (net) 
					& " inversion mark must be at the end of the net name !",
					console => true);
				raise constraint_error;
			end if;
		end if;
		
	end check_net_name_characters;
	
	function length (segment : in type_net_segment_base) return type_distance is
	-- Returns the length of the given net segment.
		len : type_distance;
		use et_string_processing;
	begin
		len := distance (segment.coordinates_start, segment.coordinates_end);
		--log (text => "segment length " & et_coordinates.to_string (len) & "mm", level => 3);
		return len;
	end length;
	
	function to_string (segment : in type_net_segment_base; scope : in type_scope := sheet) return string is
	-- Returns the start and end coordinates of the given net segment.
	begin
		return (" start"
			& to_string (position => segment.coordinates_start, scope => scope)
			& " end" 
			& to_string (position => segment.coordinates_end, scope => xy));
	end to_string;



	function to_component_reference (
	-- Converts a string like "IC303" to a composite type_component_reference.
	-- If allow_special_character_in_prefix is given true, the first character
	-- is allowed to be a special character. This is currently a kicad requirement.
	-- NOTE: Leading zeroes in the id are removed.
	-- CS: text prefix characters against character set component_prefix_characters
		text_in : in string;
		allow_special_character_in_prefix : in boolean := false -- CS: provide CAD system specific character set instead
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
-- 		et_string_processing.log ("to component reference >" & text_in & "<");
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
-- 							et_string_processing.log ("false");							
							if is_upper(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;

						when true =>
-- 							et_string_processing.log ("true");
							if is_upper(c) or is_special(c) then -- CS: test for et_kicad.schematic_component_power_symbol_prefix instead.
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

	function default_component_reference return type_component_reference is
	-- Returns a default component reference with an empty prefix and and id 0.
	-- Used to initialize a component reference.	
	begin
		return ((
			prefix		=> type_component_prefix.to_bounded_string (""),
			id			=> component_reference_id_default,
			id_width	=> 1));
	end default_component_reference;
		
	function compare_reference (left, right : in type_component_reference) return boolean is
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


	function to_string (mirror : in type_mirror) return string is
	-- returns the given mirror style as string
	begin
		return "mirror style " & to_lower (type_mirror'image (mirror));
	end to_string;
	
	function show_danger (danger : in type_danger) return string is
		preamble : constant string (1..9) := " RISK OF ";
	begin
		case danger is
			when floating_input		=> return preamble & "FLOATING INPUT(S) !";
			when contention			=> return preamble & "CONTENTION !";
			when short_circuit		=> return preamble & "SHORT CIRCUIT OR OVERLOAD !";
			when no_power_supply	=> return preamble & "COMPONENT DAMAGE !";
			when not_predictable	=> return preamble & "UNPREDICTABLE HARM !";
		end case;	
	end show_danger;
	


	procedure statistics_set (
		cat			: in type_statistics_category;
		increment	: in boolean := true;
		number 		: in count_type := 0) is 
	begin
		if increment then
			case cat is
				when COMPONENTS_TOTAL		=> statistics.components_total		:= statistics.components_total + 1;
				when COMPONENTS_VIRTUAL		=> statistics.components_virtual	:= statistics.components_virtual + 1;
				when COMPONENTS_REAL		=> statistics.components_real		:= statistics.components_real + 1;
				when COMPONENTS_MOUNTED		=> statistics.components_mounted	:= statistics.components_mounted + 1;
				
				when NETS_TOTAL				=> statistics.nets_total			:= statistics.nets_total + 1;
				when JUNCTIONS				=> statistics.junctions				:= statistics.junctions + 1;
				when PORTS_TOTAL			=> statistics.ports_total			:= statistics.ports_total + 1;
				
				when CAPACITORS				=> statistics.capacitors			:= statistics.capacitors + 1;
				when CONNECTORS				=> statistics.connectors			:= statistics.connectors + 1;
				when DIODES					=> statistics.diodes				:= statistics.diodes + 1;
				when INDUCTORS				=> statistics.inductors				:= statistics.inductors + 1;
				when INTEGRATED_CIRCUITS	=> statistics.integrated_circuits	:= statistics.integrated_circuits + 1;
				when JUMPERS				=> statistics.jumpers				:= statistics.jumpers + 1;
				when LEDS					=> statistics.leds					:= statistics.leds + 1;
				when NETCHANGERS			=> statistics.netchangers			:= statistics.netchangers + 1;
				when RELAYS					=> statistics.relays				:= statistics.relays + 1;
				when RESISTORS				=> statistics.resistors				:= statistics.resistors + 1;
				when TESTPOINTS				=> statistics.testpoints			:= statistics.testpoints + 1;				
				when TRANSISTORS			=> statistics.transistors			:= statistics.transistors + 1;
			end case;
		else
			case cat is
				when COMPONENTS_TOTAL	=> statistics.components_total := number;
				when COMPONENTS_VIRTUAL	=> statistics.components_virtual := number;
				
				when others => null; -- CS
			end case;
		end if;
	end statistics_set;
		
	function statistics_query (cat : in type_statistics_category) return count_type is
	-- Returns the number objects as specified by given category.
	begin
		case cat is
			when COMPONENTS_TOTAL		=> return statistics.components_total;
			when COMPONENTS_VIRTUAL		=> return statistics.components_virtual;
			when COMPONENTS_REAL		=> return statistics.components_real;
			when COMPONENTS_MOUNTED		=> return statistics.components_mounted;
			
			when NETS_TOTAL				=> return statistics.nets_total;
			when JUNCTIONS				=> return statistics.junctions;
			when PORTS_TOTAL			=> return statistics.ports_total;
			
			when CAPACITORS				=> return statistics.capacitors;
			when CONNECTORS				=> return statistics.connectors;
			when DIODES					=> return statistics.diodes;
			when INDUCTORS				=> return statistics.inductors;
			when INTEGRATED_CIRCUITS	=> return statistics.integrated_circuits;
			when JUMPERS				=> return statistics.jumpers;
			when LEDS					=> return statistics.leds;
			when NETCHANGERS			=> return statistics.netchangers;
			when RELAYS					=> return statistics.relays;
			when RESISTORS				=> return statistics.resistors;
			when TESTPOINTS				=> return statistics.testpoints;				
			when TRANSISTORS			=> return statistics.transistors;
		end case;

	end statistics_query;

	function statistics_query (cat : in type_statistics_category) return string is
	-- Returns the number objects as specified by given category.
	begin
		return count_type'image (statistics_query (cat));
	end statistics_query;

	
end et_schematic;
-- Soli Deo Gloria

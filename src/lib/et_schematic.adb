------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET SCHEMATIC                              --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab width in your edtior to 4.

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
with et_csv;

package body et_schematic is

	function to_net_label_text_size (text : in string) return type_net_label_text_size is
	-- Converts a string to type_net_label_text_size.
	begin
		return type_net_label_text_size'value (text);
	end to_net_label_text_size;


-- 	procedure write_note_properties (
-- 		note 			: in et_schematic.type_text;
-- 		log_threshold	: in et_string_processing.type_log_level := 0) is
-- 	-- Writes the properties of the given note
-- 		use et_string_processing;
-- 		use et_coordinates;
-- 		use et_libraries;
-- 	begin
-- 		log ("text note" & to_string (position => note.coordinates, scope => xy), log_threshold);
-- 
-- 		log_indentation_up;
-- 
-- 		-- content
-- 		if et_libraries.type_text_content.length (note.content) > 0 then
-- 			log (text => "content '" & type_text_content.to_string (note.content) & "'", level => log_threshold);
-- 		else
-- 			log (text => et_string_processing.message_warning & "no content !", level => log_threshold); 
-- 		end if;
-- 	
-- 		if log_level >= log_threshold + 1 then
-- 			
-- 			-- size
-- 			log ("size" & et_libraries.type_text_size'image (note.size));
-- 
-- 			-- style
-- 			log ("style " & to_lower(et_libraries.type_text_style'image (note.style)));
-- 
-- 			-- line width
-- 			log ("line width" & et_libraries.type_text_line_width'image (note.line_width));
-- 
-- 			-- rotation
-- 			log (to_string (note.rotation));
-- 
-- 			-- visible
-- 			--log ("visible " & to_lower (et_libraries.type_text_visible'image (note.visible)));
-- 
-- 			-- alignment
-- 			log ("alignment (hor/vert) "
-- 				& to_lower (et_libraries.type_text_alignment_horizontal'image (note.alignment.horizontal))
-- 				& "/"
-- 				& to_lower (et_libraries.type_text_alignment_vertical'image (note.alignment.vertical)));
-- 
-- 		end if;
-- 		
-- 		log_indentation_down;
-- 	end write_note_properties;

	function to_string (appearance : in type_net_label_appearance) return string is begin
		return latin_1.space & to_lower (type_net_label_appearance'image (appearance));
	end to_string;

	function to_appearance (appearance : in string) return type_net_label_appearance is begin
		return type_net_label_appearance'value (appearance);
	end to_appearance;
	
	function to_string (direction : in type_net_label_direction) return string is begin
		return latin_1.space & to_lower (type_net_label_direction'image (direction));
	end to_string;

	function to_direction (direction : in string) return type_net_label_direction is begin
		return type_net_label_direction'value (direction);
	end to_direction;
	
-- 	function length (segment : in type_net_segment_base) return et_coordinates.type_distance is
-- 	-- Returns the length of the given net segment.
-- 		use et_coordinates;
-- 		len : type_distance;
-- 		use et_string_processing;
-- 	begin
-- 		len := distance (segment.coordinates_start, segment.coordinates_end);
-- 		--log (text => "segment length " & et_coordinates.to_string (len) & "mm", level => 3);
-- 		return len;
-- 	end length;
	
-- 	function to_string (
-- 		segment	: in type_net_segment_base'class;
-- 		scope 	: in et_coordinates.type_scope := et_coordinates.sheet)
-- 		return string is
-- 	-- Returns the start and end coordinates of the given net segment.
-- 		use et_coordinates;
-- 	begin
-- 		return (" start"
-- 			& to_string (position => segment.coordinates_start, scope => scope)
-- 			& " end" 
-- 			& to_string (position => segment.coordinates_end, scope => xy));
-- 	end to_string;

	function to_string (net_scope : in type_net_scope) return string is
	begin
		return " " & to_lower (type_net_scope'image (net_scope));
	end to_string;

	function to_net_scope (scope : in string) return type_net_scope is
	begin
		return type_net_scope'value (scope);
	end to_net_scope;

	function default_component_reference return et_libraries.type_component_reference is
	-- Returns a default component reference with an empty prefix and and id 0.
	-- Used to initialize a component reference.	
		use et_libraries;
	begin
		return ((
			prefix		=> type_component_prefix.to_bounded_string (""),
			id			=> component_reference_id_default,
			id_width	=> 1));
	end default_component_reference;
		
	function compare_reference (left, right : in et_libraries.type_component_reference) return boolean is
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

	function equal_reference (left, right : in et_libraries.type_component_reference) return boolean is
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

	function to_string (
		mirror	: in type_mirror;
		verbose : in boolean)
		return string is
	-- returns the given mirror style as string
	begin
		if verbose then
			return "mirrored " & to_lower (type_mirror'image (mirror));
		else
			return latin_1.space & to_lower (type_mirror'image (mirror));
		end if;
	end to_string;

	function to_mirror_style (style : in string) return type_mirror is begin
		return type_mirror'value (style);
	end to_mirror_style;

	procedure validate_purpose (purpose : in string) is
	-- Raises alarm if purpose is empty, purpose_default or nonsense.
		use et_string_processing;
		procedure purpose_invalid is
		begin
			log_indentation_reset;
			log (message_error & "purpose '" & purpose & "' not sufficiently specified !",
				 console => true);
			raise constraint_error;
		end purpose_invalid;

		place : natural := 0;
	begin -- validate_purpose

		-- test for the default string
		if purpose = purpose_default then
			purpose_invalid;
		end if;

		-- test if length is non-zero
		if purpose'length = 0 then 
			log_indentation_reset;
			log (message_error & "no purpose specified !",
				 console => true);
			raise constraint_error;
		end if;

		-- test for keywords and characters
		
		place := ada.strings.fixed.count (to_lower (purpose), "purpose");
		if place > 0 then purpose_invalid; end if;

		place := ada.strings.fixed.count (purpose, "?");
		if place > 0 then purpose_invalid; end if;

		-- CS: others like "to do, TODO, TBD
		
	end validate_purpose;
	
	function to_string (purpose : in type_component_purpose.bounded_string) return string is
	-- Returns the given purpose as string.
	begin
		return type_component_purpose.to_string (purpose);
	end to_string;

	function to_purpose (purpose : in string) return type_component_purpose.bounded_string is
	-- Converts a string to type_component_purpose	
	begin
		return type_component_purpose.to_bounded_string (purpose);
	end to_purpose;

	procedure check_purpose_length (purpose : in string) is
	-- Tests if the given purpose is longer than allowed.
		use et_string_processing;
	begin
		if purpose'length > component_purpose_length_max then
			log_indentation_reset;
			log (message_error & "max. number of characters for purpose is" 
				 & positive'image (component_purpose_length_max) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_purpose_length;
	
	procedure check_purpose_characters (
		purpose		: in type_component_purpose.bounded_string;
		characters	: in character_set := component_purpose_characters) is
	-- Tests if the given purpose contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
		use et_string_processing;
		use type_component_purpose;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => purpose,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "component purpose " & to_string (purpose) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_purpose_characters;

	
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
	
	function to_string (bom : in type_bom) return string is begin
		return latin_1.space & to_lower (type_bom'image (bom));
	end to_string;

	function to_bom_status (bom : in string) return type_bom is begin
		return type_bom'value (bom);
	end to_bom_status;
	
	procedure check_bom_characters (bom : in string) is
	-- Checks if given string is a bom status. Case sensitive ! 
		use et_string_processing;
	begin
		if bom = type_bom'image (YES) then
			null;
		elsif bom = type_bom'image (NO) then
			null;
		else
			log_indentation_reset;
			log (message_error & "BOM status '"
					& bom & "' invalid !" 
					& " Must be either "
					& to_string (YES) & " or "
					& to_string (NO) & " !",
				console => true);
			raise constraint_error;
		end if;

		-- CS: warning if lower case used
	end check_bom_characters;
	
	procedure validate_bom_status (text : in string) is -- CS: see spec
	-- Validates BOM status. Case sensitive !
		use et_string_processing;
	begin
		if text = type_bom'image (YES) then
			null;
		elsif text = type_bom'image (NO) then
			null;
		else
			log_indentation_reset;
			log (message_error & "BOM status '"
					& text & "' invalid !" 
					& " Must be either "
					& to_string (YES) & " or "
					& to_string (NO) & " !",
				console => true);
			raise constraint_error;
		end if;
	end validate_bom_status;

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
-- 				when NETCHANGERS			=> statistics.netchangers			:= statistics.netchangers + 1;
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
-- 			when NETCHANGERS			=> return statistics.netchangers;
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

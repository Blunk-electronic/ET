------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                NETS                                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.vectors;

with et_string_processing;		use et_string_processing;

package et_nets is

	-- If the name of a strand can not be identified,
	-- we default to the well proven "N$" notation:
	anonymous_net_name_prefix : constant string := "N$";
	subtype type_anonymous_net_index is positive range 1 .. 1_000_000;
	-- CS increase upper limit if necessary.
	
	-- The name of a net may have 100 characters which seems sufficient for now.
	net_name_characters : character_set := to_set (ranges => (('A','Z'),('0','9'))) or to_set ("_-#");

	net_inversion_mark : constant string := "#"; 
	-- CS required ? should not be allowed at all. Operater should write net_name_N
	-- instead.

	net_name_length_max : constant natural := 100;

	package pac_net_name is new generic_bounded_length (net_name_length_max); 
	use pac_net_name;



	
	
	procedure check_net_name_length (net : in string);
	-- Tests if the given net name is longer than allowed.
	
	procedure check_net_name_characters (
		net			: in pac_net_name.bounded_string;
		characters	: in character_set := net_name_characters);
	-- Tests if the given net name contains only valid characters as specified
	-- by given character set.

	-- Returns true if given net name is empty:
	function is_empty (net : in pac_net_name.bounded_string)
		return boolean;
	
	function to_net_name (net_name : in string) return pac_net_name.bounded_string;

	
	-- Returns a name for an anonymous net like N$56
	function to_anonymous_net_name (index : in type_anonymous_net_index) 
		return pac_net_name.bounded_string;
	
	function anonymous (net_name : in pac_net_name.bounded_string) return boolean;
	-- Returns true if the given net name is anonymous.





-- INDEXED NETS

	-- Sometimes a list of net names along with an index is
	-- required in a form like:

	--     net       |  index
	-- ------------------------
	-- analog_input  |      1
	-- digital_out   |      2
	-- gnd           |      3
	-- zero_pressure |    109

	-- The GUI requires this format in order to list net names in
	-- comboboxes.
	
	-- The index is defined as:	
	subtype type_net_index is positive range 1 .. 1_000_000;
	-- CS increase upper limit if necessary.
	
	-- Plain net names can be collected in a vector.
	-- A vector is used in order to get a consequtive index.
	package pac_net_names_indexed is new vectors (
		index_type		=> type_net_index,
		element_type	=> pac_net_name.bounded_string);

	type type_net_indexed is private;

	-- This function returns the index of an indexed net:
	function get_index (net : in type_net_indexed) return type_net_index;

	-- This function returns the name of an indexed net:
	function get_name (net : in type_net_indexed) return pac_net_name.bounded_string;

	
	-- This procedure "builds" an indexed net.
	-- WARNING ! There is no check whether the net name
	-- and the index match ! Fox example: You can build an indexed
	-- net named "analog_input" with an index "999" and
	-- nothing would prevent you from doing so.
	procedure set (
		net 	: in out type_net_indexed;
		name	: in pac_net_name.bounded_string;
		idx		: in type_net_index := type_net_index'first);

	-- Returns true if the given indexed net has been
	-- initialized. "Initialized" means, it has a
	-- non-empty net name.
	function is_initialized (net : in type_net_indexed)
		return boolean;
	

	private
		type type_net_indexed is record
			name	: pac_net_name.bounded_string;
			idx		: type_net_index := type_net_index'first;
		end record;
	
end et_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              NET NAMES                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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


with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;

with ada.strings.bounded;       use ada.strings.bounded;

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with et_logging;				use et_logging;


package et_net_names is

	
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

	
	no_name : constant pac_net_name.bounded_string := to_bounded_string ("");

	

	function net_name_to_string (
		net_name	: in pac_net_name.bounded_string)
		return string;

	
	
	-- Tests if the given net name is longer than allowed.
	procedure check_net_name_length (net : in string);


	
	-- Tests if the given net name contains only valid characters as specified
	-- by given character set.
	procedure check_net_name_characters (
		net			: in pac_net_name.bounded_string;
		characters	: in character_set := net_name_characters);


	
	-- Returns true if given net name is empty:
	function is_empty (net : in pac_net_name.bounded_string)
		return boolean;

	
	function to_net_name (net_name : in string) return pac_net_name.bounded_string;


	
	
	-- Returns a name for an anonymous net like N$56
	function to_anonymous_net_name (index : in type_anonymous_net_index) 
		return pac_net_name.bounded_string;


	
	-- Returns true if the given net name is anonymous.
	-- CS rename to is_anonymous
	function anonymous (net_name : in pac_net_name.bounded_string) return boolean;


	-- Changes the given net name to an empty string:
	procedure clear_net_name (
		net_name : in out pac_net_name.bounded_string);
	
	
	-- Net names can also be collected in simple lists:
	package pac_net_names is new doubly_linked_lists (pac_net_name.bounded_string);

	no_net_names : constant pac_net_names.list := pac_net_names.empty_list;


	
	-- Returns the name of the net indicated by the given cursor as string:
	function to_string (
		net	: in pac_net_names.cursor)
		return string;

	
end et_net_names;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

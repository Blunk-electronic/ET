------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PACKAGE NAMES                                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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
--                                                                          --
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

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;


package et_package_names is


	-- A package (or a footprint) is something like "SOT32" or "NDIP14". 
	-- It is a more or less standardized (JEDEC)
	-- designator for the housing or the case of an electronical component.
	-- The package name is independed of
	-- the actual purpose of a device. An LED can have an SOT23 package and
	-- a transistor can also come in an SOT23.

	-- Package names like "SOT23" or "TO220" are stored in bounded strings:
	package_name_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) 
		or to_set('.')
		or to_set('-')
		or to_set('_'); 

	package_name_length_max : constant positive := 100;
	package pac_package_name is new generic_bounded_length (package_name_length_max);

	function to_string (packge : in pac_package_name.bounded_string) return string;
	-- Returns the given package name as as string.
	-- CS: provide a parameter that turns the preamble on/off

	function to_package_name (package_name : in string) return pac_package_name.bounded_string;
	-- Converts a string to a pac_package_name.
	
	procedure check_package_name_length (packge : in string);
	-- Tests if the given package name is longer than allowed.
	
	procedure check_package_name_characters (
		packge		: in pac_package_name.bounded_string;
		characters	: in character_set := package_name_characters);
	-- Tests if the given package name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.



	

	package_model_file_name_length_max : constant positive := 300;
	package pac_package_model_file_name is new generic_bounded_length (package_model_file_name_length_max);

	package_model_file_extension : constant string := "pac";
	
	use pac_package_model_file_name;
	
	function to_string (name : in pac_package_model_file_name.bounded_string) return string;
	function to_file_name (name : in string) return pac_package_model_file_name.bounded_string;

	
	
	
end et_package_names;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PACKAGE VARIANT NAME                            --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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


with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;


package et_package_variant_name is


	-- The variant is usually a suffix in a device value, given by its manufacturer. The variant is a manufacturer
	-- specific abbrevation for the package a device comes with.
	-- Example: An opamp made by TI can be the type TL084N or TL084D. 
	-- N indicates a NDIP14 package, whereas D stands for the SO14 package.
	-- If a device has package variants, a suffix after the value indicates 
	-- the package name.
	-- The variant name is manufacturer specific. example: TL084D or TL084N.
	-- Device package variant names like "N" or "D" are stored in bounded strings:
	variant_name_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) or to_set ("_-");

	
	variant_name_length_max : constant positive := 50;
	
	package pac_package_variant_name is new 
		generic_bounded_length (variant_name_length_max);
	
	use pac_package_variant_name;

	

	no_variant : constant pac_package_variant_name.bounded_string :=
		pac_package_variant_name.to_bounded_string ("");


	

	-- Returns true if the given variant name is empty:
	function is_empty (
		variant_name : in pac_package_variant_name.bounded_string)
		return boolean;


	
	-- function to_string (
	-- 	variant : in pac_package_variant_name.bounded_string) 
	-- 	return string;

	
	function to_variant_name (
		variant_name : in string) 
		return pac_package_variant_name.bounded_string;

	
	-- tests if the given variant name is not longer than allowed
	procedure check_variant_name_length (
		variant_name : in string);
	
	
	-- Tests if the given variant name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
	procedure check_variant_name_characters (
		variant		: in pac_package_variant_name.bounded_string;
		characters	: in character_set := variant_name_characters);
	
	
end et_package_variant_name;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

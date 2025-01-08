------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICES NAMES                                  --
--                                                                          --
--                              S p e c                                     --
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
with et_device_prefix;			use et_device_prefix;



package et_device_name is

	
	prefix_default : constant pac_device_prefix.bounded_string := pac_device_prefix.to_bounded_string ("?");

	
	subtype type_name_index is natural range natural'first .. 99_999; -- R1..R99999, IC1..IC99999 should be enough

	name_index_default : constant type_name_index := 0;

	
	function to_string (index : in type_name_index) return string;

	function to_index (index : in string) return type_name_index;

	
	subtype type_index_width is positive range positive'first .. 5; -- see number of digits of type_device_name_index


	
	type type_device_name is record -- CS: should be private
		prefix		: pac_device_prefix.bounded_string := prefix_default; -- like "IC"
		id			: type_name_index := name_index_default; -- like "303"
		id_width	: type_index_width := type_index_width'first; -- the number of digits of the id. 
		-- Example: id_width is 3 in case of an id like of 937
		-- NOTE: This allows something like R091 or IC0 (there are reasons for such strange things ...)
	end record;


	
	
	-- Returns true if the prefixes of left and right are equal:
	function same_prefix (left, right : in type_device_name) return boolean;


	
	-- Converts a string like "IC303" to a composite type_device_name.
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the index are removed. R002 becomes R2.
	function to_device_name (text_in : in string) return type_device_name;



	-- Returns true if left comes before right.
	-- If left equals right, the return is false.	
	function "<" (left, right : in type_device_name) return boolean;


	
	-- Returns true if left equals right.
	function "=" (left, right : in type_device_name) return boolean;


	no_name : constant type_device_name := (others => <>);
	

	
	-- Returns the given device name as string.
	-- Prepends leading zeros according to name.id_width.
	function to_string (name : in type_device_name) return string;

	
	-- Returns the prefix of the given device name.
	function get_prefix (
		name : in type_device_name) 
		return pac_device_prefix.bounded_string;

	
	-- Returns the index of the given device name.
	function get_index (
		name : in type_device_name) 
		return type_name_index;


	
	-- Builds a device name by given prefix (like R) and index (like 23) to a device name (like R23).
	-- If width is not provided, then the width of the index is calculated automatically. In case of R23 the width is 2.
	-- If width is provided, then it is set accordingly.
	function to_device_name (
		prefix	: in pac_device_prefix.bounded_string; 	-- R, C, L
		index	: in type_name_index;				-- 1, 20, ..
		width	: in type_index_width := type_index_width'first) -- the number of digits
		return type_device_name;


	
	-- Adds to the device index the given offset. 
	-- Example: given name is R4, given offset is 100. Result R104.
	procedure offset_index (
		name	: in out type_device_name;
		offset	: in type_name_index);

		
end et_device_name;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

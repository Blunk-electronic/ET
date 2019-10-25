------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        COMPONENT LIBRARIES                               --
--                                                                          --
--                             S p e c                                      --
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
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;
--with ada.containers.vectors;

with et_coordinates;			use et_coordinates;
with et_geometry;
with et_string_processing;
with et_general;
with et_text;
-- with et_symbols;				use et_symbols;
-- with et_packages;				use et_packages;
with et_devices;				use et_devices;

package et_libraries is

	path_length_max : constant natural := 500; -- CS: increase if necessary


	
	
	-- The name of the person who has drawn, checked or approved something may have 100 characters which seems sufficient for now.
 	person_name_length : constant natural := 100;
	package type_person_name is new generic_bounded_length (person_name_length);

	function to_string (person : in type_person_name.bounded_string) return string;
	-- Returns the given person name as string.









	

	


	
-- DEVICE NAMES
	keyword_device : constant string := "device";
	
	type type_device_name_element is (PREFIX, ID);
	device_name_prefix_default : constant type_device_name_prefix.bounded_string := type_device_name_prefix.to_bounded_string ("?");

	subtype type_device_name_index is natural range natural'first .. 99_999; -- R1..R99999, IC1..IC99999 should be enough
	device_name_index_default : constant type_device_name_index := 0;

	function to_string (index : in type_device_name_index) return string;
	function to_device_name_index (index : in string) return type_device_name_index;

	subtype type_device_name_index_width is positive range positive'first .. 5; -- see number of digits of type_device_name_index
	
	type type_device_name is record -- CS: should be private
		prefix		: type_device_name_prefix.bounded_string := device_name_prefix_default; -- like "IC"
		id			: type_device_name_index := device_name_index_default; -- like "303"
		id_width	: type_device_name_index_width; -- the number of digits of the id. 3 in case of an id of 303 -- CS default ?
		-- NOTE: This allows something like R091 or IC0 (there are reasons for such strange things ...)
	end record;

	function to_device_name (
	-- Converts a string like "IC303" to a composite type_device_name.
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the index are removed. R002 becomes R2.
		text_in : in string)
		return type_device_name;

	function "<" (left, right : in type_device_name) return boolean;
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.	

	function equal_name (left, right : in type_device_name) return boolean;
	-- Returns true if left equals right.
	
	function to_string (name : in type_device_name) return string;
	-- Returns the given device name as string.
	-- Prepends leading zeros according to name.id_width.
	
	function prefix (name : in type_device_name) return type_device_name_prefix.bounded_string;
	-- Returns the prefix of the given device name.

	function index (name : in type_device_name) return type_device_name_index;
	-- Returns the index of the given device name.

	function to_device_name (
		prefix	: in type_device_name_prefix.bounded_string; 	-- R, C, L
		index	: in type_device_name_index;					-- 1, 20, ..
		width	: in type_device_name_index_width := type_device_name_index_width'first) -- the number of digits
		return type_device_name;

	procedure offset_device_name (
	-- Adds to the device index the given offset. 
	-- Example: given name is R4, given offset is 100. Result R104.
		name	: in out type_device_name;
		offset	: in type_device_name_index);
	
	


	
-- PACKAGES
	-- CS move to et_package
	
	-- A package is something like "SOT32" or "NDIP14". It is a more or less standardized (JEDEC)
	-- designator for the housing or the case of an electronical component. The package name is independed of
	-- the actual purpose of a device. An LED can have an SOT23 package and a transistor can also come in an SOT23.

	-- device package names like "SOT23" or "TO220" are stored in bounded strings:
	component_package_name_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) 
		or to_set('.')
		or to_set('_'); 

	component_package_name_length_max : constant positive := 100;
	package type_component_package_name is new generic_bounded_length (component_package_name_length_max);

	function to_string (packge : in type_component_package_name.bounded_string) return string;
	-- Returns the given package name as as string.
	-- CS: provide a parameter that turns the preamble on/off

	function to_package_name (package_name : in string) return type_component_package_name.bounded_string;
	-- Converts a string to a type_component_package_name.
	
	procedure check_package_name_length (packge : in string);
	-- Tests if the given package name is longer than allowed.
	
	procedure check_package_name_characters (
		packge		: in type_component_package_name.bounded_string;
		characters	: in character_set := component_package_name_characters);
	-- Tests if the given package name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
	




	





	
-- DRAWING FRAME

	-- $ET_FRAMES/drawing_frame_version_1.frm
	frame_template_name_length_max : constant positive := 300;
	package type_frame_template_name is new generic_bounded_length (frame_template_name_length_max);

	frame_template_name_dummy : constant type_frame_template_name.bounded_string := 
		type_frame_template_name.to_bounded_string ("dummy_frame");
	
	function to_string (name : in type_frame_template_name.bounded_string) return string;
	function to_template_name (name : in string) return type_frame_template_name.bounded_string;
	
    type type_title_block_line is null record; -- CS
-- 		coordinates_start : type_point;
-- 		coordinates_end   : type_point;
--     end record;

	package type_title_block_lines is new doubly_linked_lists (type_title_block_line);
    
 	title_block_text_length_max : constant natural := 200;
	package type_title_block_text_content is new generic_bounded_length (title_block_text_length_max);

	type type_title_block_text_meaning is ( 
		PROJECT, TITLE, 
        DRAWN_BY, CHECKED_BY, APPROVED_BY, 
        DRAWN_DATE, CHECKED_DATE, APPROVED_DATE,
        COMPANY,
		REVISION, MISC);
	
	type type_title_block_text is record
		meaning			: type_title_block_text_meaning;
-- CS 		coordinates		: type_point; 
		text			: type_title_block_text_content.bounded_string; -- CS: rename to content
 		size			: natural; -- CS pac_text.type_text_size;
 		rotation		: et_coordinates.type_rotation;
		-- CS: font, ...
 	end record;

	package type_title_block_texts is new doubly_linked_lists (type_title_block_text);

    -- the final title block
    type type_title_block is record
--  CS       coordinates     : type_point; -- CS rename to position
        lines           : type_title_block_lines.list;
        texts           : type_title_block_texts.list;
    end record;

    -- A drawing frame consists of straight lines and texts.
    -- The text is a character at the x/y border that helps to locate objects.
    type type_frame_line is null record; -- CS
-- 		coordinates_start : type_point;
--         coordinates_end   : type_point;
-- 	end record;
	
	package type_frame_lines is new doubly_linked_lists (type_frame_line);

	type type_frame_text is record
-- CS		coordinates		: type_point; -- CS rename to position
		text			: character_set := et_string_processing.general_characters; -- CS rename to content
		size			: natural; -- CS pac_text.type_text_size;
		rotation		: et_coordinates.type_rotation;
		-- CS: font, ...
	end record;
	
	package type_frame_texts is new doubly_linked_lists (type_frame_text);

	-- the final drawing frame
	-- NOTE: The native drawing frame has its lower left corner at position x/y 0/0. always.
    type type_frame is tagged record
        paper_size      : et_general.type_paper_size; -- the size of the paper
        size_x, size_y  : et_coordinates.type_distance; -- the dimensions of the frame (should fit into paper_size) 
        lines           : type_frame_lines.list;
		texts           : type_frame_texts.list;
		title_block		: type_title_block;
    end record;

--     -- There are lots of drawing frames in a schematic. We store them in a vector:
-- 	package type_frames is new vectors (
-- 		index_type		=> et_coordinates.type_submodule_sheet_number,
-- 		element_type	=> type_frame);

	
		
end et_libraries;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

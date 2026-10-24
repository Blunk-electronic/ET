------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             DEVICE UNIT                                  --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
-- DESCRIPTION:
--
--  This package describes a so called "unit". A unit is a subset of an
--  electrical device. The blueprint of a unit is a symbol. As soon as
--  a symbol is instantiated in the schematic, it becomes a unit.
--  So this package is about units in the schematic and associated
--  primitive operations.
--
--   history of changes:
--
--   ToDo:

with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_port_names;						use et_port_names;
with et_symbol_ports;					use et_symbol_ports;
with et_rotation_docu;					use et_rotation_docu;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_unit_name;						use et_unit_name;
with et_mirroring;						use et_mirroring;
with et_device_appearance;				use et_device_appearance;
with et_object_status;					use et_object_status;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;


package et_device_unit is

	use pac_unit_name;

	use pac_geometry_2;



	-- In a schematic we handle only virtual devices (like GND symbols)
	-- and those which appear in both schematic an layout (so called real devices):
	subtype type_appearance_schematic is type_appearance
		range APPEARANCE_VIRTUAL .. APPEARANCE_PCB;


	-- In a schematic we find units spread all over.
	-- A unit is a subset of a device.
	-- Placeholders are available if the device appears in both schematic and layout:
	--	type type_unit (appearance : type_appearance_schematic) is record -- CS make private
	type type_unit ( -- CS: rename to type_device_unit ?
		appearance : type_appearance_schematic)
	is new type_schematic_object with record -- CS make private
		-- position		: type_object_position; -- incl. rotation and sheet number
		mirror_status	: type_mirror := MIRROR_NO;
		-- status			: type_object_status;
		case appearance is
			when APPEARANCE_VIRTUAL =>
				null; -- CS

			when APPEARANCE_PCB =>
				placeholders : type_text_placeholders;
		end case;
		-- The plaeholders have only x/y positions relative
		-- to the unit position.
	end record;



	function is_real (
		unit : in type_unit)
		return boolean;



	-- Builds a unit. If appearance is virtual, then
	-- the given placeholders are ignored:
	function to_unit (
		appearance		: in type_appearance_schematic;
		object			: in type_schematic_object;
		mirror			: in type_mirror;
		placeholders	: in type_text_placeholders := default_text_placeholders)
		return type_unit;


	-- Returns x/y position of the given unit:
	-- function get_place (
	-- 	unit	: in type_unit)
	-- 	return type_vector_model;



	-- function get_position (
	-- 	unit	: in type_unit)
	-- 	return type_object_position;


	-- procedure set_position (
	-- 	unit		: in out type_unit;
	-- 	position	: in type_object_position);





	function get_mirror_status (
		unit	: in type_unit)
		return type_mirror;


	function get_mirror_status (
		unit	: in type_unit)
		return string;


	procedure set_mirror_status (
		unit	: in out type_unit;
		mirror	: in type_mirror);


	procedure toggle_mirror_status (
		unit	: in out type_unit);






	-- function get_rotation (
	-- 	unit	: in type_unit)
	-- 	return type_rotation_model;


	-- -- Sets the absolute rotation of a unit:
	-- procedure set_rotation (
	-- 	unit		: in out type_unit;
	-- 	rotation	: in type_rotation_model);


	-- Rotates the unit by a given angle:
	-- procedure rotate_by (
	-- 	unit		: in out type_unit;
	-- 	rotation	: in type_rotation_model);



	-- function get_sheet (
	-- 	unit	: in type_unit)
	-- 	return type_sheet;

	-- procedure set_sheet (
	-- 	unit	: in out type_unit;
	-- 	sheet	: in type_sheet);



	-- -- Moves a unit by the given offset:
	-- procedure move_unit (
	-- 	unit	: in out type_unit;
	-- 	offset	: in type_vector_model);


	-- -- Moves a unit by the given number of sheets:
	-- procedure move_unit (
	-- 	unit	: in out type_unit;
	-- 	offset	: in type_sheet_relative);




	-- Returns true if the given unit is in
	-- the given catch zone:
	-- function in_catch_zone (
	-- 	unit	: in type_unit;
	-- 	zone	: in type_catch_zone;
	-- 	sheet	: in type_sheet)
	-- 	return boolean;



	-- -- Returns true if the given unit is
	-- -- on the given sheet and in the given
	-- -- rectangular area:
	-- function on_sheet_and_in_area (
	-- 	unit	: in type_unit;
	-- 	sheet	: in type_sheet;
	-- 	area	: in type_area)
	-- 	return boolean;

	-- -- Returns true if the given unit is
	-- -- on the given sheet and if it is selected:
	-- function on_sheet_and_selected (
	-- 	unit	: in type_unit;
	-- 	sheet	: in type_sheet)
	-- 	return boolean;


	procedure unit_not_found (
		name : in type_unit_name);




	-- procedure set_selected (
	-- 	unit : in out type_unit);

	-- procedure clear_selected (
	-- 	unit : in out type_unit);

	-- function is_selected (
	-- 	unit : in type_unit)
	-- 	return boolean;

	-- procedure set_proposed (
	-- 	unit : in out type_unit);

	-- procedure clear_proposed (
	-- 	unit : in out type_unit);

	-- function is_proposed (
	-- 	unit : in type_unit)
	-- 	return boolean;




	-- procedure set_moving (
	-- 	unit : in out type_unit);

	-- procedure clear_moving (
	-- 	unit : in out type_unit);

	-- function is_moving (
	-- 	unit : in type_unit)
	-- 	return boolean;

	-- procedure modify_status (
	-- 	unit		: in out type_unit;
	-- 	operation	: in type_status_operation);



	-- procedure reset_status (
	-- 	unit : in out type_unit);




-- PLACEHOLDERS:

	-- Returns the placeholders of the unit.
	-- If the unit is virtual, then default placeholders
	-- are returned:
	function get_placeholders (
		unit : in type_unit)
		return type_text_placeholders;


	-- Moves the placeholder given by meaning.
	-- If coordinates is absolute, then the placeholder
	-- is moved to the given point.
	-- If coordinates is relative, then the placeholder
	-- is moved by the x/y-distance given by point:
	procedure move_placeholder (
		unit		: in out type_unit;
		meaning		: in type_placeholder_meaning;
		coordinates	: in type_coordinates; -- relative/absolute
		point		: in type_vector_model); -- x/y



	-- Rotates the placeholder given by meaning.
	-- If toggle is true, then the rotation toggles between
	-- horizonal and vertical. Otherwise, the rotation is
	-- set as given by rotation:
	procedure rotate_placeholder (
		unit		: in out type_unit;
		meaning		: in type_placeholder_meaning;
		toggle		: in boolean;
		rotation	: in type_rotation_documentation);


end et_device_unit;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16

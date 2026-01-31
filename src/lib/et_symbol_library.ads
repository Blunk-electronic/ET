------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     SCHEMATIC SYMBOL LIBRARY                             --
--                                                                          --
--                              S p e c                                     --
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
-- DESCRIPTION:
--
--  This package is about so called "symbols". A symbol is an abstraction
--  of an electrical component like a resistor, capactor, inductor or
--  a subset of an integrated circuit.
--
--   history of changes:
--

with ada.containers; 			use ada.containers;
with ada.containers.indefinite_ordered_maps;

with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_geometry;				use et_schematic_geometry;

with et_logging;						use et_logging;
with et_symbol_model;					use et_symbol_model;
with et_symbol_name;					use et_symbol_name;
with et_device_appearance;				use et_device_appearance;
with et_string_processing;				use et_string_processing;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;


package et_symbol_library is

	use pac_geometry_2;


	-- Symbol models are stored in files ending with *.sym.
	-- At the same time a
	-- symbol name (like "libraries/symbols/nand.sym")
	-- is also the key to the symbol library:
	
	package pac_symbol_models is new indefinite_ordered_maps (
		key_type		=> pac_symbol_model_name.bounded_string,
		"<"				=> pac_symbol_model_name."<",
		element_type	=> type_symbol);

	use pac_symbol_models;



	
	
	-- THIS IS THE RIG WIDE LIBRARY OF SYMBOLS MODELS:
	
	symbol_library : pac_symbol_models.map;



	function get_symbol_model_name (
		symbol_cursor : in pac_symbol_models.cursor)
		return pac_symbol_model_name.bounded_string;
	

	function get_symbol_model_name (
		symbol_cursor : in pac_symbol_models.cursor)
		return string;

	

	-- Creates a symbol and stores it in symbol library:
	procedure create_symbol (
		symbol_name		: in pac_symbol_model_name.bounded_string;
		appearance		: in type_appearance;
		log_threshold	: in type_log_level);


	
	
	-- Returns for a given symbol model file name
	-- (like ../libraries/symbols/nand.sym)
	-- the symbol model in the symbol library.
	-- If the symbol can not be located then cursor is
	-- set to no_element:
	procedure get_symbol_model ( -- CS rename to set_symbol_model
		model_file	: in pac_symbol_model_name.bounded_string; -- CS rename to model_name
		cursor		: in out pac_symbol_models.cursor);


	-- Returns for a given symbol model file name
	-- (like ../libraries/symbols/nand.sym)
	-- the symbol model in the symbol library.
	-- If the symbol can not be located then cursor is
	-- set to no_element:
	function get_symbol_model (
		model_name : in pac_symbol_model_name.bounded_string)
		return pac_symbol_models.cursor;

	
		
	-- Returns true if the given symbol will be part of a real device:
	function is_real (
		symbol : in pac_symbol_models.cursor)
		return boolean;


	-- Returns the x/y-positions of the given symbol:
	function get_port_positions (
		symbol	: in pac_symbol_models.cursor)
		return pac_points.list;



	-- Returns the placeholders of a symbol.
	-- If the symbol represents a virtual device,
	-- then default placeholders are returned:
	function get_placeholders (
		symbol	: in pac_symbol_models.cursor)
		return type_text_placeholders;


	-- In the symbol, the placeholders have a rotation (about itself)
	-- and a position relative to the origin of the symbol.
	-- On instanciating a symbol in the schematic, it becomes a unit
	-- which may have a rotation of its own.
	-- This function translates from the rotation of placeholders
	-- described in the symbol model to the rotation of
	-- placeholders of a unit in the schematic.
	-- It translates according to the rotation given by destination:
	function get_default_placeholders (
		symbol_cursor	: in pac_symbol_models.cursor;
		destination		: in type_object_position) -- x/y/rotation of the unit
		return type_text_placeholders;


	
	function get_symbol (
		symbol	: in pac_symbol_models.cursor)
		return type_symbol;

							
end et_symbol_library;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

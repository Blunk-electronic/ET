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

with et_schematic_geometry;				use et_schematic_geometry;

with et_logging;						use et_logging;
with et_symbol_model;					use et_symbol_model;
with et_symbol_name;					use et_symbol_name;
with et_device_appearance;				use et_device_appearance;
with et_string_processing;				use et_string_processing;


package et_symbol_library is

	use pac_geometry_2;


	
	package pac_symbols is new indefinite_ordered_maps (
		key_type		=> pac_symbol_model_file.bounded_string, -- ../libraries/symbols/NAND.sym
		"<"				=> pac_symbol_model_file."<",
		element_type	=> type_symbol);

	use pac_symbols;



	
	-- THIS IS THE RIG WIDE LIBRARY OF SYMBOLS:
	
	symbol_library : pac_symbols.map;


	-- Creates a symbol and stores it in container symbols.
	procedure create_symbol (
		symbol_name		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		appearance		: in type_appearance;
		log_threshold	: in type_log_level);


	
	
	
	-- Locates the symbol model in the rig wide symbol library 
	-- by the given file name. Set the cursor accordingly.
	-- If the model has not been found, then the cursor is
	-- set to no_element:
	procedure locate_symbol (
		model_file	: in pac_symbol_model_file.bounded_string;  -- ../libraries/symbols/NAND.sym
		cursor		: in out pac_symbols.cursor);
	
		
	-- Returns true if the given symbol will be part of a real device:
	function is_real (
		symbol : in pac_symbols.cursor)
		return boolean;


	-- Returns the x/y-positions of the given symbol:
	function get_port_positions (
		symbol	: in pac_symbols.cursor)
		return pac_points.list;


end et_symbol_library;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

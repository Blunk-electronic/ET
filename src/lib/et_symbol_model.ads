------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      SCHEMATIC SYMBOL MODEL                              --
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

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_schematic_geometry;				use et_schematic_geometry;

with et_logging;						use et_logging;
with et_symbol_shapes;					use et_symbol_shapes;
with et_symbol_name;					use et_symbol_name;
with et_symbol_ports;					use et_symbol_ports;
with et_symbol_text;					use et_symbol_text;
with et_device_appearance;				use et_device_appearance;


package et_symbol_model is

	use pac_geometry_2;


	
	type type_symbol_base is tagged record		
		texts : pac_symbol_texts.list; -- the collection of texts
	end record;

	
	
	type type_symbol (appearance : type_appearance) 
	is new type_symbol_base with 
		record
		shapes	: type_shapes; -- the collection of shapes
		ports	: pac_symbol_ports.map;
		
		case appearance is
			when APPEARANCE_PCB =>
				-- Placeholders to be filled with content when 
				-- a symbol is instantiated:
				placeholders : type_default_placeholders;

			when APPEARANCE_VIRTUAL => null;				
		end case;
	end record;

	

	-- Returns true if the given symbol will be part of a real device:
	function is_real (
		symbol : in type_symbol)
		return boolean;



	
	-- Retrurns x/y-positions the the ports of the given symbol:
	function get_port_positions (
		symbol	: in type_symbol)
		return pac_points.list;


	
	-- Returns the placeholders of the symbol.
	-- If the symbol represents a virtual device,
	-- then default placeholders are returned:
	function get_placeholders (
		symbol	: in type_symbol)
		return type_default_placeholders;
	

end et_symbol_model;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

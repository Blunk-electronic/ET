------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        UNITS OF MEASUREMENT                              --
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
--   history of changes:
--

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;


with et_logging;					use et_logging;




package et_units_of_measurement is


	-- CS apply prefix 

	
	type type_unit_of_measurement is (
		MILLIOHM,
		OHM,
		KILOOHM,
		MEGAOHM,
		GIGAOHM,

		PICOFARAD,
		NANOFARAD,
		MICROFARAD,
		MILLIFARAD,
		FARAD,
		
		NANOHENRY,
		MICROHENRY,		
		MILLIHENRY,	
		HENRY,

		VOLT,

		MILLIAMPERE,
		AMPERE,

		KILOHERTZ,
		MEGAHERTZ,
		GIGAHERTZ
		);


	
	-- Converts a string to type_unit_of_measurement:
	function to_unit_of_measurement (
		unit : in string) 
		return type_unit_of_measurement;

	
	-- Returns the given unit of measurement as string. (things like OHM, KILOOHM, MEGAOHM, ...)
	function to_string (
		unit : in type_unit_of_measurement) 
		return string;

	
	-- The abbrevations of units of measurement are limited to two characters.
	-- So the user could define units like uF or mH. More than two characters are not common.
	-- However, the recommendation is to use just one character like u, m, k, M. Reason: how to express
	-- something like 3.3Ohms since the Ohm character is a special character ?
	-- By the category of the component we can reason that it is about Ohms, Henry or Farad.
	unit_abbrevation_characters : character_set := to_set (ranges => (('A','Z'),('a','z')));
	unit_abbrevation_length_max : constant positive := 2;


	
	package pac_unit_abbrevation is new generic_bounded_length (unit_abbrevation_length_max);

	use pac_unit_abbrevation;


	-- Units of measurement and their abbrevation are stored in a map:
	package pac_units_of_measurement is new ordered_maps (
		key_type 		=> type_unit_of_measurement, -- OHMS, KILOOHM, MEGAOHM, ...
		element_type	=> pac_unit_abbrevation.bounded_string); -- R, m, k, ...


	
	
	-- Tests if the given abbrevation contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
	procedure check_abbrevation_of_unit_characters (
		abbrevation	: in pac_unit_abbrevation.bounded_string;
		characters	: in character_set);
	


	
	
		
end et_units_of_measurement;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

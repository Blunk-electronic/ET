------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          DEVICE CATEGORY                                 --
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


with et_logging;				use et_logging;



package et_device_category is

	
	-- CS apply prefix
	-- category_prefix	: constant string := ("CAT_");


	type type_device_category is (
		ANTENNA,
		BATTERY,
		BUZZER,
-- 		CABLE,
		CAPACITOR,
		CAPACITOR_ADJUSTABLE,	-- adjustable capacitor (also known as trimmer)
		CONNECTOR,				-- component where another component of opposide gender is plugged
		DIAC,
		DIODE,
		DIODE_PHOTO,			-- light sensitive diode
		DISPLAY,				-- display, LCD, LED, VFD, ...
		FIDUCIAL,				-- non-electric device
		FUSE,
		HEATSINK,				-- a bulk of metal that absorbs and dissipates excessive heat
		INDUCTOR,
		INDUCTOR_ADJUSTABLE,
		INTEGRATED_CIRCUIT,
		JUMPER,					-- a component that allows tieing nets via a removable bridge
		KEYPAD,					-- array of push buttons, keys, switches, ...
		LIGHT_EMMITTING_DIODE,	-- an LED, LASER-diode, IRED-LED, ...
		LIGHT_EMMITTING_DIODE_ARRAY,
		LOUDSPEAKER,
		MICROPHONE,
		NETCHANGER,				-- ties two nets together
		MOTOR,
		MOUNTING_HOLE,			-- non-electric device
		OPTOCOUPLER,
		QUARTZ,					-- quartz crystal resonators
		POTENTIOMETER,			-- variable resistor
		RELAY,
		RESISTOR,				-- varistors, trimmers, potentiometers, photoresistors
		RESISTOR_ADJUSTABLE,	-- adjustable resistor
		RESISTOR_NETWORK,		-- a collection of resistors in a single housing
		RESISTOR_PHOTO,			-- light sensitive resistor
		SWITCH,					-- push buttons, breakers, makers, rotary encoders, ...
		TESTPOINT,				-- a point where measurements can be taken
		THYRISTOR,
		THYRISTOR_PHOTO,		-- light sensitive thyristor
		TRANSFORMER,
		TRANSISTOR,				-- NPN, PNP, NFET, MOSFET, ...
		TRANSISTOR_PHOTO,		-- light sensitive transistor
		TRIAC,
		TUBE,					-- triodes, pentodes, thyratrons, klystrons, ...
		UNKNOWN					-- not specified
-- 		WIRE
		);





	-- Returns the given component category as string:
	function to_string (cat : in type_device_category) return string;


	
	-- Converts a string to a type_device_category:
	function to_category (category : in string) return type_device_category;

	

	
		
end et_device_category;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

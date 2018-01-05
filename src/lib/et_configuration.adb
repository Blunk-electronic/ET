------------------------------------------------------------------------------
--                                                                          --
--                        SYSTEM ET CONFIGURATION                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

-- with ada.characters;			use ada.characters;
-- with ada.characters.latin_1;	use ada.characters.latin_1;
-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings; 				use ada.strings;
-- with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

-- with ada.containers;            use ada.containers;
-- with ada.containers.indefinite_ordered_maps;


with ada.directories;

with et_general;
with et_libraries;
with et_string_processing;		use et_string_processing;


package body et_configuration is

	procedure create_default_configuration (file_name : in type_configuration_file_name.bounded_string) is
	-- Creates a default configuration file.
	begin

		create (
			file => configuration_file_handle, 
			mode => out_file, 
			name => et_configuration.type_configuration_file_name.to_string (file_name));

		
		null;

		
	end create_default_configuration;

		
end et_configuration;

-- Soli Deo Gloria

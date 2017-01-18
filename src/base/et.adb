------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET MODULE BASE                            --
--                                                                          --
--                                 M-1                                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   Mario.Blunk@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Integer_Text_IO;		use Ada.Integer_Text_IO;
with Ada.Characters.Handling; 	use Ada.Characters.Handling;

with Ada.Strings.Bounded; 		use Ada.Strings.Bounded;
with Ada.Strings.fixed; 		use Ada.Strings.fixed;
with Ada.Exceptions; 			use Ada.Exceptions;
 
with Ada.Command_Line;			use Ada.Command_Line;
with Ada.Directories;			use Ada.Directories;

with ada.containers; use ada.containers;
with ada.containers.vectors;


with et_schematic; 	use et_schematic;
with et_operations; use et_operations;

procedure et is

	version			: String (1..3) := "000";
	prog_position	: natural := 0;

	p : type_port;
	
begin
	null;


	p.name := type_port_name.to_bounded_string("CLK");
	p.direction := passive;
	p.coordinates.x := 0;
	p.coordinates.y := 0;

	
end et;

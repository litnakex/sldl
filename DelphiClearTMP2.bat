@echo off
call :RemoveByExt .dof
call :RemoveByExt .dcu
call :RemoveByExt .~pas
call :RemoveByExt .~dpr
call :RemoveByExt .~ddp
call :RemoveByExt .~dfm
call :RemoveByExt .~def
call :RemoveByExt .ddp
goto :EOF

:RemoveByExt
@echo Remove files *%1
for /f "delims==" %%A in ('dir . /b /s') do (
	if /I "%%~xA" EQU "%1" (
		del "%%A"
	)
)

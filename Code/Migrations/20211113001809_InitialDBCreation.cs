using System;
using Microsoft.EntityFrameworkCore.Migrations;

namespace DDDNetCore.Migrations
{
    public partial class InitialDBCreation : Migration
    {
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Categories",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Description = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Active = table.Column<bool>(type: "bit", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Categories", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Connections",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Player = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Friend = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    ConnectionStrength_Strength = table.Column<int>(type: "int", nullable: true),
                    Active = table.Column<bool>(type: "bit", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Connections", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "DirectRequests",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Player = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Target = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    PlayerToTargetMessage_Text = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    CurrentStatus_CurrentStatus = table.Column<int>(type: "int", nullable: true),
                    Strength_Strength = table.Column<int>(type: "int", nullable: true),
                    Active = table.Column<bool>(type: "bit", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DirectRequests", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Families",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Description = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Active = table.Column<bool>(type: "bit", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Families", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "IntroductionRequests",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    MiddleMan = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    PlayerToMiddleManMessage_Text = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    MiddleManToTargetMessage_Text = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Player = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Target = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    PlayerToTargetMessage_Text = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    CurrentStatus_CurrentStatus = table.Column<int>(type: "int", nullable: true),
                    Strength_Strength = table.Column<int>(type: "int", nullable: true),
                    Active = table.Column<bool>(type: "bit", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_IntroductionRequests", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Missions",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Challenger = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Objective = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Difficulty_Difficulty = table.Column<int>(type: "int", nullable: true),
                    CurrentStatus_CurrentStatus = table.Column<int>(type: "int", nullable: true),
                    Active = table.Column<bool>(type: "bit", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Missions", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Players",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Name_name = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Email_address = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    DateOfBirth_date = table.Column<DateTime>(type: "datetime2", nullable: true),
                    PhoneNumber_phoneNumber = table.Column<double>(type: "float", nullable: true),
                    EmotionalStatus_Status = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Facebook_Url = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    LinkedIn_Url = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Active = table.Column<bool>(type: "bit", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Players", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Products",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Description = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    CategoryId = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Active = table.Column<bool>(type: "bit", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Products", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Connections_Tags",
                columns: table => new
                {
                    ConnectionId = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Id = table.Column<int>(type: "int", nullable: false)
                        .Annotation("SqlServer:Identity", "1, 1"),
                    tagName = table.Column<string>(type: "nvarchar(max)", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Connections_Tags", x => new { x.ConnectionId, x.Id });
                    table.ForeignKey(
                        name: "FK_Connections_Tags_Connections_ConnectionId",
                        column: x => x.ConnectionId,
                        principalTable: "Connections",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "DirectRequests_Tags",
                columns: table => new
                {
                    DirectRequestId = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Id = table.Column<int>(type: "int", nullable: false)
                        .Annotation("SqlServer:Identity", "1, 1"),
                    tagName = table.Column<string>(type: "nvarchar(max)", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DirectRequests_Tags", x => new { x.DirectRequestId, x.Id });
                    table.ForeignKey(
                        name: "FK_DirectRequests_Tags_DirectRequests_DirectRequestId",
                        column: x => x.DirectRequestId,
                        principalTable: "DirectRequests",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "IntroductionRequests_Tags",
                columns: table => new
                {
                    IntroductionRequestId = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Id = table.Column<int>(type: "int", nullable: false)
                        .Annotation("SqlServer:Identity", "1, 1"),
                    tagName = table.Column<string>(type: "nvarchar(max)", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_IntroductionRequests_Tags", x => new { x.IntroductionRequestId, x.Id });
                    table.ForeignKey(
                        name: "FK_IntroductionRequests_Tags_IntroductionRequests_IntroductionRequestId",
                        column: x => x.IntroductionRequestId,
                        principalTable: "IntroductionRequests",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });
        }

        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "Categories");

            migrationBuilder.DropTable(
                name: "Connections_Tags");

            migrationBuilder.DropTable(
                name: "DirectRequests_Tags");

            migrationBuilder.DropTable(
                name: "Families");

            migrationBuilder.DropTable(
                name: "IntroductionRequests_Tags");

            migrationBuilder.DropTable(
                name: "Missions");

            migrationBuilder.DropTable(
                name: "Players");

            migrationBuilder.DropTable(
                name: "Products");

            migrationBuilder.DropTable(
                name: "Connections");

            migrationBuilder.DropTable(
                name: "DirectRequests");

            migrationBuilder.DropTable(
                name: "IntroductionRequests");
        }
    }
}

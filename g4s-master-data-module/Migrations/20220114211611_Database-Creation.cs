using System;
using Microsoft.EntityFrameworkCore.Migrations;

namespace DDDNetCore.Migrations
{
    public partial class DatabaseCreation : Migration
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
                    Player = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    Friend = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    ConnectionStrength_Strength = table.Column<int>(type: "int", nullable: false),
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
                    Player = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    Target = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    PlayerToTargetMessage_Text = table.Column<string>(type: "nvarchar(1000)", maxLength: 1000, nullable: false),
                    CurrentStatus_CurrentStatus = table.Column<int>(type: "int", nullable: false),
                    Strength_Strength = table.Column<int>(type: "int", nullable: false),
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
                    MiddleMan = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    PlayerToMiddleManMessage_Text = table.Column<string>(type: "nvarchar(1000)", maxLength: 1000, nullable: false),
                    MiddleManToTargetMessage_Text = table.Column<string>(type: "nvarchar(1000)", maxLength: 1000, nullable: false),
                    Player = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    Target = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    PlayerToTargetMessage_Text = table.Column<string>(type: "nvarchar(1000)", maxLength: 1000, nullable: false),
                    CurrentStatus_CurrentStatus = table.Column<int>(type: "int", nullable: false),
                    Strength_Strength = table.Column<int>(type: "int", nullable: false),
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
                    Challenger = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    Objective = table.Column<string>(type: "nvarchar(70)", maxLength: 70, nullable: false),
                    Difficulty_Difficulty = table.Column<int>(type: "int", nullable: false),
                    CurrentStatus_CurrentStatus = table.Column<int>(type: "int", nullable: false),
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
                    Email_address = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Password_password = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    DateOfBirth_date = table.Column<DateTime>(type: "datetime2", nullable: false),
                    PhoneNumber_phoneNumber = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    EmotionalStatus_Status = table.Column<int>(type: "int", nullable: false),
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
                    tagName = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false)
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
                    tagName = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false)
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
                    tagName = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false)
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

            migrationBuilder.CreateTable(
                name: "Players_Tags",
                columns: table => new
                {
                    PlayerId = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Id = table.Column<int>(type: "int", nullable: false)
                        .Annotation("SqlServer:Identity", "1, 1"),
                    tagName = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Players_Tags", x => new { x.PlayerId, x.Id });
                    table.ForeignKey(
                        name: "FK_Players_Tags_Players_PlayerId",
                        column: x => x.PlayerId,
                        principalTable: "Players",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateIndex(
                name: "IX_Players_Email_address",
                table: "Players",
                column: "Email_address",
                unique: true);
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
                name: "Players_Tags");

            migrationBuilder.DropTable(
                name: "Products");

            migrationBuilder.DropTable(
                name: "Connections");

            migrationBuilder.DropTable(
                name: "DirectRequests");

            migrationBuilder.DropTable(
                name: "IntroductionRequests");

            migrationBuilder.DropTable(
                name: "Players");
        }
    }
}
